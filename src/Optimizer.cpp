#include "Optimizer.h"

#include "llvm/Pass.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>

using namespace llvm;
extern bool debug_mode;

struct OptimizerPass : public FunctionPass {
	static char ID;
	OptimizerPass() : FunctionPass(ID) {}

	struct Edge {
		BasicBlock *origin;
		Value *cond = nullptr;
		bool condV;
	};

	struct ValueDomain { // @warning overflow behavior not yet taken into account
		bool isBottom = true;
		bool isDead = false;
		int min = INT_MIN, max = INT_MAX;

		static ValueDomain join(ValueDomain &a, ValueDomain &b) {
			if (a.isBottom) return b;
			if (b.isBottom) return a;

			ValueDomain result;
			result.isBottom = false;
			result.min = std::min(a.min, b.min);
			result.max = std::max(a.max, b.max);
			return result;
		}

		static ValueDomain top(bool isDead) {
			ValueDomain result;
			result.isDead = isDead;
			result.isBottom = false;
			return result;
		}

		bool contains(int v) const { return !isBottom && v >= min && v <= max; }
		bool isConstant() const { return !isBottom && min == max; }
		bool isTop() const { return !isBottom && min == INT_MIN && max == INT_MAX; }

		bool operator==(const ValueDomain &other) const {
			return other.isBottom == isBottom && other.min == min && other.max == max && other.isDead == isDead;
		}
	};

	struct ConstraintSet {
		bool isBottom = false; // bottom = can never become true
		bool isTop() const { return !isBottom && predicates.empty(); }

		bool isPure = true; // all contained predicates are equivalent

		static CmpInst::Predicate oppositePredicate(CmpInst::Predicate pred) {
			switch (pred) {
			case ICmpInst::ICMP_NE: return ICmpInst::ICMP_EQ;
			case ICmpInst::ICMP_EQ: return ICmpInst::ICMP_NE;
			case ICmpInst::ICMP_SGE: return ICmpInst::ICMP_SLT;
			case ICmpInst::ICMP_SLT: return ICmpInst::ICMP_SGE;
			case ICmpInst::ICMP_SGT: return ICmpInst::ICMP_SLE;
			case ICmpInst::ICMP_SLE: return ICmpInst::ICMP_SGT;

			default:
				std::cerr << "unsupported comparison" << std::endl;
				exit(1);
			}
		}

		static CmpInst::Predicate swappedPredicate(CmpInst::Predicate pred) {
			switch (pred) {
			case ICmpInst::ICMP_SGE: return ICmpInst::ICMP_SLE;
			case ICmpInst::ICMP_SLT: return ICmpInst::ICMP_SGT;
			case ICmpInst::ICMP_SGT: return ICmpInst::ICMP_SLT;
			case ICmpInst::ICMP_SLE: return ICmpInst::ICMP_SGE;
			default: return pred;
			}
		}

		static bool conflict(CmpInst::Predicate a, CmpInst::Predicate b) { // x <a> y <=> !(x <b> y)
			return _conflict(a, b) || _conflict(b, a);
		}

		static bool entails(CmpInst::Predicate a, CmpInst::Predicate b) { // x <a> y => x <b> y
			if (a == b) return true;
			switch (a) {
			case ICmpInst::ICMP_EQ: return b == ICmpInst::ICMP_SGE || b == ICmpInst::ICMP_SLE;
			case ICmpInst::ICMP_SLE: return b == ICmpInst::ICMP_SLT;
			case ICmpInst::ICMP_SGE: return b == ICmpInst::ICMP_SGT;
			default: return false;
			}
		}

	protected:
		static bool _conflict(CmpInst::Predicate a, CmpInst::Predicate b) {
			if (a == b) return false;
			switch (a) {
			case ICmpInst::ICMP_NE: return b == ICmpInst::ICMP_EQ;
			case ICmpInst::ICMP_EQ: return b == ICmpInst::ICMP_SGT || b == ICmpInst::ICMP_SLT || b == ICmpInst::ICMP_NE;
			case ICmpInst::ICMP_SGE: return b == ICmpInst::ICMP_SLT;
			case ICmpInst::ICMP_SLT: return b == ICmpInst::ICMP_SGE || b == ICmpInst::ICMP_SGT || b == ICmpInst::ICMP_EQ;
			case ICmpInst::ICMP_SLE: return b == ICmpInst::ICMP_SGT;
			case ICmpInst::ICMP_SGT: return b == ICmpInst::ICMP_SLE || b == ICmpInst::ICMP_SLT || b == ICmpInst::ICMP_EQ;
			case CmpInst::BAD_ICMP_PREDICATE: return false;

			default:
				std::cerr << "unsupported comparison" << std::endl;
				exit(1);
			}
		}

	public:
		static CmpInst::Predicate join(CmpInst::Predicate a, CmpInst::Predicate b) {
			// assume no conflict
			if (a == b) return a;

			switch (a) {
			case CmpInst::ICMP_NE:
				if (b == ICmpInst::ICMP_SGE) return ICmpInst::ICMP_SGT;
				if (b == ICmpInst::ICMP_SLE) return ICmpInst::ICMP_SLT;
				return b;

			case CmpInst::ICMP_SGE:
				if (b == ICmpInst::ICMP_SLE) return ICmpInst::ICMP_EQ;
				if (b == ICmpInst::ICMP_NE) return ICmpInst::ICMP_SGT;
				return b;

			case CmpInst::ICMP_SLE:
				if (b == ICmpInst::ICMP_SGE) return ICmpInst::ICMP_EQ;
				if (b == ICmpInst::ICMP_NE) return ICmpInst::ICMP_SLT;
				return b;

			case CmpInst::BAD_ICMP_PREDICATE: return b;
			default: return join(b, a);
			}
		}

		void add(Value *lhs, Value *rhs, CmpInst::Predicate pred) {
			_add(lhs, rhs, pred);
			_add(rhs, lhs, swappedPredicate(pred));
		}

		CmpInst::Predicate get(Value *lhs, Value *rhs) {
			if (lhs == rhs) return ICmpInst::ICMP_EQ;
			auto it = predicates.find(std::make_pair(lhs, rhs));
			if (it == predicates.end()) return CmpInst::BAD_ICMP_PREDICATE;
			return it->second;
		}

		// @todo merge+entailment, becomes impure

		std::map<std::pair<Value *, Value *>, CmpInst::Predicate> predicates;

	protected:
		void set(Value *lhs, Value *rhs, CmpInst::Predicate pred) {
			predicates[std::make_pair(lhs, rhs)] = pred;
		}

		void _add(Value *lhs, Value *rhs, CmpInst::Predicate pred) {
			auto prevPred = get(lhs, rhs);
			if (conflict(prevPred, pred)) {
				isBottom = true;
				return;
			}

			set(lhs, rhs, join(prevPred, pred));

			auto cmp = dyn_cast<CmpInst>(lhs);
			auto cint = dyn_cast<ConstantInt>(rhs);

			if (cmp && cint) {
				auto subPred = cmp->getPredicate();
				bool inv = false;

				switch (pred) {
				case llvm::CmpInst::ICMP_EQ: inv = cint->isZero(); break;
				case llvm::CmpInst::ICMP_NE: inv = !cint->isZero(); break;
				default:
					std::cerr << "comparison comparison not supported" << std::endl;
					return;
				}

				if (inv) subPred = oppositePredicate(subPred);

				add(
					cmp->getOperand(0),
					cmp->getOperand(1),
					subPred
				);
			}
		}
	};

	struct BlockDomain {
		ConstraintSet cs;

		std::vector<Edge> edges;
		bool isEntry = false, reachable = false;

		bool operator==(const BlockDomain &other) {
			return other.isEntry == isEntry && other.reachable == reachable;
		}
	};

	std::map<BasicBlock *, BlockDomain> blocks;
	std::map<Value *, ValueDomain> values;

	void dumpAnalysis();
	bool trackValue(Value *v, BasicBlock *block);

	bool iterate(llvm::Function &func) {
		bool hasChanged = false;

		// update reachability
		for (auto &b : blocks) {
			auto prevBd = b.second;
			auto &bd = b.second;

			if (bd.isEntry) continue;

			bd.reachable = false;
			for (auto &edge : bd.edges) {
				if (!blocks[edge.origin].reachable) continue;
				if (!edge.cond || values[edge.cond].contains(edge.condV)) {
					bd.reachable = true;
					break;
				}
			}

			if (!(prevBd == bd)) {
				if (debug_mode) std::cout << "change: block " << std::string(b.first->getName()) << std::endl;
				hasChanged = true;
			}
		}

		// update domains for all variables
		for (auto &block : func.getBasicBlockList())
			for (auto &inst : block.getInstList())
				hasChanged = hasChanged || trackValue(&inst, &block);

		return hasChanged;
	}

	void fixPHINodes(llvm::Function &func) {
		for (auto &block : func.getBasicBlockList()) {
			std::vector<PHINode *> phiNodes;
			for (auto &inst : block.getInstList())
				if (auto phi = dyn_cast<PHINode>(&inst)) phiNodes.push_back(phi);

			for (auto &phi : phiNodes) {
				for (int i = 0; i < phi->getNumIncomingValues();)
					if (!blocks[phi->getIncomingBlock(i)].reachable) phi->removeIncomingValue(i);
					else ++i;

				if (phi->getNumIncomingValues() == 1) {
					phi->replaceAllUsesWith(phi->getIncomingValue(0));
					phi->eraseFromParent();
				}
			}
		}
	}

	void fixConstants(llvm::Function &func) {
		for (auto &block : func.getBasicBlockList()) {
			std::vector<Instruction *> constants;
			for (auto &inst : block.getInstList())
				if (values[&inst].isConstant()) constants.push_back(&inst);

			for (auto &constant : constants) {
				Value *newValue = llvm::ConstantInt::get(constant->getType(), values[constant].min);
				constant->replaceAllUsesWith(newValue);
				constant->eraseFromParent();
			}
		}
	}

	void fixBranches(llvm::Function &func) {
		for (auto &block : func.getBasicBlockList()) {
			std::vector<BranchInst *> branches; // conditional branches
			for (auto &inst : block.getInstList())
				if (auto br = dyn_cast<BranchInst>(&inst))
					if (br->isConditional()) branches.push_back(br);

			for (auto &branch : branches)
				if (auto c = dyn_cast<ConstantInt>(branch->getCondition())) {
					auto newBranch = BranchInst::Create(branch->getSuccessor(c->isZero() ? 1 : 0));
					newBranch->insertAfter(branch);
					branch->eraseFromParent();
				} /* else if (branch->getSuccessor(0) == branch->getSuccessor(1)) {
					auto newBranch = BranchInst::Create(branch->getSuccessor(0));
					newBranch->insertAfter(branch);
					branch->eraseFromParent();
				} */
		}
	}

	void removeDeadCode(llvm::Function &func) {
		for (auto &block : func.getBasicBlockList()) {
			std::vector<Instruction *> dead;
			for (auto &inst : block.getInstList())
				if (values[&inst].isDead) dead.push_back(&inst);

			for (auto &d : dead) d->eraseFromParent();
		}
	}

	void removeUnreachableCode() { // @todo name simplifyCFG
		for (auto &b : blocks) {
			auto &block = b.first;
			auto &bd = b.second;

			if (!bd.reachable) {
				block->eraseFromParent();
				continue;
			}

			if (block->getInstList().size() > 1) continue;

			auto branch = dyn_cast<BranchInst>(&block->getInstList().front());
			if (!branch || branch->isConditional()) continue; // might be a return

			auto replacement = branch->getSuccessor(0);
			if (bd.isEntry && blocks[replacement].edges.size() > 1)
				// cannot replace entry with block that has predecessors
				continue;

			block->replaceAllUsesWith(replacement);
			block->eraseFromParent();
		}
	}

	// @todo @important CSE analysis
	ValueDomain getVD(Value *value, BasicBlock *block) {
		if (auto ci = dyn_cast<llvm::ConstantInt>(value)) {
			int intVal = (int)*ci->getValue().getRawData();

			ValueDomain vd;
			vd.isBottom = false;
			vd.min = intVal;
			vd.max = intVal;
			return vd;
		}

		return values[value];
	}

	bool runOnFunction(llvm::Function &func) override {
		blocks.clear();
		values.clear();

		auto &entry = blocks[&func.getEntryBlock()];
		entry.reachable = true;
		entry.isEntry = true;

		for (auto &block : func.getBasicBlockList()) {
			auto term = block.getTerminator();

			Edge edge;
			edge.origin = &block;
			edge.condV = 1;

			if (auto br = dyn_cast<BranchInst>(term))
				if (br->isConditional()) edge.cond = br->getCondition();

			for (auto succ : term->successors()) {
				if (edge.cond) {
					if (auto cmp = dyn_cast<ICmpInst>(edge.cond)) {
						auto pred = cmp->getPredicate();
						if (!edge.condV) pred = ConstraintSet::oppositePredicate(pred);

						blocks[succ].cs.add(
							cmp->getOperand(0),
							cmp->getOperand(1),
							pred
						);
					} else {
						blocks[succ].cs.add(
							edge.cond,
							ConstantInt::get(edge.cond->getType(), 0),
							edge.condV ? ICmpInst::ICMP_NE : ICmpInst::ICMP_EQ
						);
					}
				}
				
				blocks[succ].edges.push_back(edge);
				edge.condV = 0;
			}
		}

		//if (debug_mode) dumpAnalysis();

		int it = 0;
		while (iterate(func)) {
			if (++it > 1000) {
				std::cerr << "aborting optimization" << std::endl;
				return false;
			}

			/*
			if (debug_mode) {
				std::cout << "\n\niterated..." << std::endl;
				dumpAnalysis();
			}
			*/
		}

		if (debug_mode) dumpAnalysis();

		// @todo combine the following
		fixPHINodes(func);
		fixConstants(func);
		fixBranches(func);
		removeDeadCode(func);

		removeUnreachableCode();

		return true;
	}
};

std::ostream &operator<<(std::ostream &os, OptimizerPass::ValueDomain const &vd) {
	if (vd.isDead) os << "dead ";
    if (vd.isBottom) return os << "bottom";
    if (vd.isTop()) return os << "top";
    return os << "[" << vd.min << ";" << vd.max << "]";
}

std::ostream &operator<<(std::ostream &os, OptimizerPass::ConstraintSet const &cs) {
	for (auto &pred : cs.predicates) {
		pred.first.first->print(errs());
		switch (pred.second) {
		case llvm::CmpInst::ICMP_EQ:  os << " == "; break;
		case llvm::CmpInst::ICMP_NE:  os << " != "; break;
		case llvm::CmpInst::ICMP_SLE: os << " <= "; break;
		case llvm::CmpInst::ICMP_SLT: os << " < ";  break;
		case llvm::CmpInst::ICMP_SGE: os << " >= "; break;
		case llvm::CmpInst::ICMP_SGT: os << " > ";  break;
		default: os << " ? ";
		}
		pred.first.second->print(errs());
		os << std::endl;
	}

	return os;
}

void OptimizerPass::dumpAnalysis() {
	for (auto &bd : blocks) {
		std::cout << "block: " << std::string(bd.first->getName()) << (bd.second.reachable ? " (1)" : " (0)") << std::endl;
		std::cout << bd.second.cs << std::endl;

		/*for (auto &edge : bd.second.edges) {
			std::cout << "  from: " << std::string(edge.origin->getName()) << std::endl;
			if (edge.cond)
				std::cout << "    cond: " << std::string(edge.cond->getName()) << " = " << (edge.condV ? "true" : "false") << std::endl;
		}*/
	}

	for (auto &v : values) {
		auto &vd = v.second;
		if (!v.first->hasName()) continue;

		std::cout << "value " << std::string(v.first->getName()) << ": " << vd << std::endl;
		//v.first->print(errs());
	}
}

bool OptimizerPass::trackValue(Value *v, BasicBlock *block) {
	auto prevVd = values[v];
	auto &vd = values[v];

	vd.isDead = true;
	if (blocks[block].reachable) {
		bool hasSideEffect = !(
			dyn_cast<Constant>(v) ||
			dyn_cast<BinaryOperator>(v) ||
			dyn_cast<ICmpInst>(v) || // we can do this, div-by-zero is undefined behavior!
			dyn_cast<PHINode>(v) ||
			dyn_cast<SelectInst>(v)
		);

		//v->print(errs()); std::cerr << " " << (hasSideEffect ? "se" : "ne") << std::endl;
		if (hasSideEffect) vd.isDead = false;
		else { // check if we're referenced
			for (auto &use : v->uses()) {
				//std::cerr << "used by "; use.getUser()->print(errs()); std::cerr << std::endl;
				if (!values[use.getUser()].isDead) {
					vd.isDead = false;
					break;
				}
			}
		}
	}

	if (vd.isDead) {
		//if (dyn_cast<Instruction>(v)) v->print(errs());
		// don't need to update domain, nobody cares about this value 😟
	} else if (!isa<User>(v) || isa<TerminatorInst>(v)) {
		// cannot be referenced, do not analyse
	} else if (
		dyn_cast<GetElementPtrInst>(v) ||
		dyn_cast<LoadInst>(v) ||
		dyn_cast<StoreInst>(v) ||
		dyn_cast<GlobalValue>(v) ||
		dyn_cast<CallInst>(v) ||
		dyn_cast<Constant>(v)
	) {
		vd = ValueDomain::top(false);
	} else if (auto add = dyn_cast<BinaryOperator>(v)) {
		auto lhs = getVD(add->getOperand(0), block);
		auto rhs = getVD(add->getOperand(1), block);

		if (lhs.isTop() || lhs.isBottom) vd = lhs;
		else if (rhs.isTop() || rhs.isBottom) vd = rhs;
		else {
			switch (add->getOpcode()) {
			case llvm::Instruction::Add:
				vd.isBottom = false;
				vd.min = lhs.min + rhs.min;
				vd.max = lhs.max + rhs.max;
				break;

			default:
				if (debug_mode) std::cerr << "unsupported binary operation" << std::endl;
				vd = ValueDomain::top(false);
			}
		}
	} else if (auto phi = dyn_cast<PHINode>(v)) {
		vd.isBottom = true;
		for (int i = 0; i < phi->getNumIncomingValues(); ++i)
			if (blocks[phi->getIncomingBlock(i)].reachable) {
				auto inVd = getVD(phi->getIncomingValue(i), block);
				vd = ValueDomain::join(vd, inVd);
			}
	} else if (auto cmp = dyn_cast<ICmpInst>(v)) {
		auto lhs = getVD(cmp->getOperand(0), block);
		auto rhs = getVD(cmp->getOperand(1), block);

		bool vTrue, vFalse, swapOut = false;
		auto cmpPred = cmp->getPredicate();
		auto entailPred = blocks[block].cs.get(cmp->getOperand(0), cmp->getOperand(1));

		if (entailPred == ICmpInst::BAD_ICMP_PREDICATE) {
			switch (cmpPred) {
			case ICmpInst::ICMP_NE: swapOut = true;
			case ICmpInst::ICMP_EQ:
				vFalse = lhs.min != lhs.max || rhs.min != rhs.max || lhs.min != rhs.min;
				vTrue = lhs.max >= rhs.min && rhs.max >= lhs.min;
				break;

			case ICmpInst::ICMP_SGE: swapOut = true;
			case ICmpInst::ICMP_SLT:
				vFalse = lhs.max >= rhs.min;
				vTrue = lhs.min < rhs.max;
				break;

			case ICmpInst::ICMP_SGT: swapOut = true;
			case ICmpInst::ICMP_SLE:
				vFalse = lhs.max > rhs.min;
				vTrue = lhs.min <= rhs.max;
				break;

			default:
				std::cerr << "unsupported comparison" << std::endl;
				exit(1);
			}
		} else {
			// got an entailing pred
			vTrue = !ConstraintSet::conflict(entailPred, cmpPred);
			vFalse = !ConstraintSet::entails(entailPred, cmpPred);
		}

		if (swapOut) std::swap(vFalse, vTrue);
		vd.isBottom = false;
		vd.min = vFalse ? 0 : 1;
		vd.max = vTrue ? 1 : 0;
	} else {
		if (debug_mode) {
			std::cerr << "unsupported instruction" << std::endl;
			v->print(errs());
		}
		vd = ValueDomain::top(false);
	}

	if (vd.isConstant())
		// the value domain has become singular, we don't need this instruction
		// (will be replaced with a constant)
		vd.isDead = true;

	if (prevVd == vd) return false;

	if (!prevVd.isBottom && !vd.isBottom && !prevVd.isTop() && !vd.isTop() && !vd.isDead) {
		if (debug_mode) std::cout << "widening" << std::endl;
		vd = ValueDomain::top(vd.isDead);
	}

	if (debug_mode) {
		std::cout << "change: value ";
		if (v->hasName()) std::cout << std::string(v->getName());
		else v->print(errs());
		std::cout << " (" << prevVd << " -> " << vd << ")" << std::endl;
	}
	return true;
}

char OptimizerPass::ID = 0;

struct CompilerResult { // @todo @fixme @important not DRY
	Compiler *compiler;
	std::vector<llvm::GlobalValue *> values;
	bool shouldExecute;

	CompilerResult(Compiler *compiler = nullptr) : compiler(compiler) {}
};

Optimizer::Optimizer(Source<CompilerResult> *source, Module *mod)
: Stream<CompilerResult, CompilerResult>(source), fpm(mod) {
	fpm.add(createPromoteMemoryToRegisterPass());
	fpm.add(new OptimizerPass());
}

bool Optimizer::next(CompilerResult *result) {
	if (this->source->next(result)) {
		for (auto &value : result->values)
			if (isa<llvm::Function>(value)) fpm.run(*cast<llvm::Function>(value));
		return true;
	} else
		return false;
}
