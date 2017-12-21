#include "Optimizer.h"
#include "Compiler.h"

#include "llvm/Pass.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

struct OptimizerPass : public FunctionPass {
	static char ID;
	OptimizerPass() : FunctionPass(ID) {}

	struct Edge {
		BasicBlock *origin;
		Value *cond = nullptr;
		bool condV;
	};

	struct ValueDomain {
		bool isBottom = false;
		int min = INT_MIN, max = INT_MAX;

		static ValueDomain join(ValueDomain &a, ValueDomain &b) {
			if (a.isBottom) return b;
			if (b.isBottom) return a;

			ValueDomain result;
			result.min = std::min(a.min, b.min);
			result.max = std::max(a.max, b.max);
			return result;
		}

		bool contains(int v) const { return !isBottom && v >= min && v <= max; }
		bool isConstant() const { return !isBottom && min == max; }

		bool operator==(const ValueDomain &other) const {
			return other.isBottom == isBottom && other.min == min && other.max == max;
		}
	};

	struct BlockDomain {
		std::vector<Edge> edges;
		bool isEntry = false, reachable = false;

		bool operator==(const BlockDomain &other) {
			return other.isEntry == isEntry && other.reachable == reachable;
		}
	};

	std::map<BasicBlock *, BlockDomain> blocks;
	std::map<Value *, ValueDomain> values;

	void dump() {
		for (auto &bd : blocks) {
			std::cout << "block: " << std::string(bd.first->getName()) << (bd.second.reachable ? " (1)" : " (0)") << std::endl;
			for (auto &edge : bd.second.edges) {
				std::cout << "  from: " << std::string(edge.origin->getName()) << std::endl;
				if (edge.cond)
					std::cout << "    cond: " << std::string(edge.cond->getName()) << " = " << (edge.condV ? "true" : "false") << std::endl;
			}
		}

		for (auto &v : values) {
			auto &vd = v.second;
			std::cout << "value: " << std::string(v.first->getName()) << " ";
			//v.first->print(errs());
			std::cout << " / min: " << vd.min << " / max: " << vd.max << std::endl;
		}
	}

	bool trackValue(Value *v) {
		if (!isa<User>(v)) return false;

		auto prevVd = values[v];
		auto &vd = values[v];

		if (auto ci = dyn_cast<llvm::ConstantInt>(v)) {
			int intVal = (int)*ci->getValue().getRawData();
			vd.min = intVal;
			vd.max = intVal;
		}

		if (auto add = dyn_cast<BinaryOperator>(v)) {
			auto &lhs = values[add->getOperand(0)];
			auto &rhs = values[add->getOperand(1)];

			switch (add->getOpcode()) {
			case llvm::Instruction::Add:
				vd.min = lhs.min + rhs.min;
				vd.max = lhs.max + rhs.max;
				break;

			default:
				std::cerr << "unsupported binary operation" << std::endl;
			}
		}

		if (auto phi = dyn_cast<PHINode>(v)) {
			vd.isBottom = true;
			for (int i = 0; i < phi->getNumIncomingValues(); ++i)
				if (blocks[phi->getIncomingBlock(i)].reachable)
					vd = ValueDomain::join(vd, values[phi->getIncomingValue(i)]);
		}

		if (auto cmp = dyn_cast<ICmpInst>(v)) {
			auto &lhs = values[cmp->getOperand(0)];
			auto &rhs = values[cmp->getOperand(1)];

			bool vTrue, vFalse, swapOut = false;

			switch (cmp->getPredicate()) {
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

			if (swapOut) std::swap(vFalse, vTrue);
			vd.min = vFalse ? 0 : 1;
			vd.max = vTrue ? 1 : 0;
		}

		return !(prevVd == vd);
	}

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

			hasChanged = hasChanged || !(prevBd == bd);
		}

		// update domains for all variables
		for (auto &block : func.getBasicBlockList()) {
			for (auto &phi : block.phis())
				for (auto &op : phi.incoming_values()) hasChanged = hasChanged || trackValue(op.get());
			for (auto &inst : block.getInstList())
				for (auto &op : inst.operands()) hasChanged = hasChanged || trackValue(op.get());
		}

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
					if (br->isConditional())
						branches.push_back(br);

			for (auto &branch : branches)
				if (auto c = dyn_cast<ConstantInt>(branch->getCondition())) {
					auto newBranch = BranchInst::Create(branch->getSuccessor(c->isZero() ? 1 : 0));
					newBranch->insertAfter(branch);
					branch->eraseFromParent();
				}
		}
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

			if (auto br = dyn_cast<BranchInst>(term)) {
				if (br->isConditional())
					edge.cond = br->getCondition();
			}

			for (auto succ : term->successors()) {
				blocks[succ].edges.push_back(edge);
				edge.condV = 0;
			}
		}

		while (iterate(func)) std::cout << "iterated..." << std::endl;

		dump();

		// @todo combine the following
		fixPHINodes(func);
		fixConstants(func);
		fixBranches(func);

		// remove unreachable code
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

			block->replaceAllUsesWith(branch->getSuccessor(0));
			block->eraseFromParent();
		}

		errs() << "Optimizer: ";
		errs().write_escaped(func.getName()) << '\n';
		return false;
	}
};

char OptimizerPass::ID = 0;

Optimizer::Optimizer(Source<CompilerResult> *source, Module *mod)
: Stream<CompilerResult, CompilerResult>(source), fpm(mod) {
	fpm.add(createPromoteMemoryToRegisterPass());
	fpm.add(new OptimizerPass());
}

bool Optimizer::next(CompilerResult *result) {
	if (this->source->next(result)) {
		for (auto &value : result->values)
			if (isa<llvm::Function>(value))
				fpm.run(*cast<llvm::Function>(value));
		return true;
	} else
		return false;
}
