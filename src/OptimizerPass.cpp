#include "OptimizerPass.h"
#include "OptimizerUtils.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/IR/Constants.h>
#include <llvm/Support/raw_ostream.h>
#pragma GCC diagnostic pop

#include <iostream>


using namespace llvm;
using namespace std;


namespace optimizer {

using ValueDomain = OptimizerPass::ValueDomain;
using BlockDomain = OptimizerPass::BlockDomain;
using ConstraintSet = OptimizerPass::ConstraintSet;
using Condition = OptimizerPass::Condition;

ValueDomain ValueDomain::join(ValueDomain &a, ValueDomain &b, bool isDead) {
	ValueDomain result;

	if (a.isBottom) result = b;
	else if (b.isBottom) result = a;
	else {
		result.type = a.type;
		result.isBottom = false;
		result.min = std::min(a.min, b.min);
		result.max = std::max(a.max, b.max);
	}

	result.isDead = isDead;
	return result;
}

ValueDomain ValueDomain::top(bool isDead, Type *type) {
	ValueDomain result;
	result.type = type;
	result.isDead = isDead;
	result.isBottom = false;
	result.makeTop();
	return result;
}

ValueDomain ValueDomain::add(Type *type, ValueDomain &lhs, ValueDomain &rhs) {
	ValueDomain result;
	result.type = type;
	result.isBottom = lhs.isBottom || rhs.isBottom;

	bool overflow = false;
	result.min = result.addOverflow(lhs.min, rhs.min, overflow);
	result.max = result.addOverflow(lhs.max, rhs.max, overflow);

	if (overflow && !(lhs.isConstant() && rhs.isConstant()))
		result.makeTop();

	return result;
}

ValueDomain ValueDomain::sub(Type *type, ValueDomain &lhs, ValueDomain &rhs) {
	ValueDomain result;
	result.type = type;
	result.isBottom = lhs.isBottom || rhs.isBottom;

	bool overflow = false;
	result.min = result.subOverflow(lhs.min, rhs.max, overflow);
	result.max = result.subOverflow(lhs.max, rhs.min, overflow);

	if (overflow && !(lhs.isConstant() && rhs.isConstant()))
		result.makeTop();

	return result;
}

ValueDomain ValueDomain::mul(Type *type, ValueDomain &lhs, ValueDomain &rhs) {
	ValueDomain result;
	result.type = type;
	result.isBottom = lhs.isBottom || rhs.isBottom;

	bool overflow = false;
	long a = result.mulOverflow(lhs.min, rhs.min, overflow);
	long b = result.mulOverflow(lhs.min, rhs.max, overflow);
	long c = result.mulOverflow(lhs.max, rhs.min, overflow);
	long d = result.mulOverflow(lhs.max, rhs.max, overflow);

	result.min = std::min({ a, b, c, d });
	result.max = std::max({ a, b, c, d });

	if (overflow && !(lhs.isConstant() && rhs.isConstant()))
		result.makeTop();

	return result;
}

ValueDomain ValueDomain::div(Type *type, ValueDomain &lhs, ValueDomain &rhs) {
	ValueDomain result;
	result.type = type;
	result.isBottom = lhs.isBottom || rhs.isBottom;

	if (rhs.isConstant() && rhs.contains(0)) {
		// division by zero!
		result.min = result.max = 0L;
		return result;
	}

	long d0 = 0, d1 = 0;

	if (rhs.min <= 0 && rhs.max <= 0) {
		d0 = d1 = std::min(rhs.max, -1L);
	} else if (rhs.min <= 0 && rhs.max > 0) {
		if (rhs.min == 0) d0 = d1 = +1;
		else {
			d0 = -1;
			d1 = +1;
		}
	} else if (rhs.min >= 0 && rhs.max >= 0) {
		d0 = d1 = std::max(rhs.min, +1L);
	}

	bool overflow = false;
	long a = result.truncate(lhs.min / d0, overflow);
	long b = result.truncate(lhs.max / d0, overflow);
	long c = result.truncate(lhs.min / d1, overflow);
	long d = result.truncate(lhs.max / d1, overflow);

	result.min = std::min({ a, b, c, d });
	result.max = std::max({ a, b, c, d });

	if (overflow && !(lhs.isConstant() && rhs.isConstant()))
		result.makeTop();

	return result;
}

long ValueDomain::addOverflow(long a, long b, bool &overflow) const {
	long result;
	overflow = __builtin_add_overflow(a, b, &result) || overflow;
	return truncate(result, overflow);
}

long ValueDomain::subOverflow(long a, long b, bool &overflow) const {
	long result;
	overflow = __builtin_sub_overflow(a, b, &result) || overflow;
	return truncate(result, overflow);
}

long ValueDomain::mulOverflow(long a, long b, bool &overflow) const {
	long result;
	overflow = __builtin_mul_overflow(a, b, &result) || overflow;
	return truncate(result, overflow);
}

long ValueDomain::truncate(long v, bool &overflow) const {
	long mask = (1UL << (bitWidth() - 1)) - 1;
	long result = v & mask;
	if (v < 0)
		result |= ~mask;

	overflow = (result != v) || overflow;
	return result;
}

bool ValueDomain::contains(int v) const { return !isBottom && v >= min && v <= max; }
bool ValueDomain::isConstant() const { return !isBottom && min == max; }
bool ValueDomain::isTop() const {
	long vmin, vmax;
	fullRange(vmin, vmax);
	return !isBottom && min == vmin && max == vmax;
}

void ValueDomain::makeTop() {
	fullRange(min, max);
}

int ValueDomain::bitWidth() const {
	if (!type)
		// for branches, @todo
		return 0;

	if (auto it = dyn_cast<IntegerType>(type))
		return it->getBitWidth();

	if (dyn_cast<PointerType>(type))
		return 64;

	// invalid type:
	return 0;
}

void ValueDomain::fullRange(long &min, long &max) const {
	if (bitWidth() < 2) {
		min = 0;
		max = bitWidth();
		return;
	}

	int bw = bitWidth();
	long v = 1UL << (bw - 1);
	min = -v;
	max = v - 1;
}

bool ValueDomain::operator==(const ValueDomain &other) const {
	return other.isBottom == isBottom && other.min == min && other.max == max && other.isDead == isDead;
}

Predicate ConstraintSet::oppositePredicate(Predicate pred) {
	switch (pred) {
	case ICmpInst::ICMP_NE: return ICmpInst::ICMP_EQ;
	case ICmpInst::ICMP_EQ: return ICmpInst::ICMP_NE;
	case ICmpInst::ICMP_SGE: return ICmpInst::ICMP_SLT;
	case ICmpInst::ICMP_SLT: return ICmpInst::ICMP_SGE;
	case ICmpInst::ICMP_SGT: return ICmpInst::ICMP_SLE;
	case ICmpInst::ICMP_SLE: return ICmpInst::ICMP_SGT;

	default:
		cerr << "unsupported comparison" << endl;
		exit(1);
	}
}

Predicate ConstraintSet::swappedPredicate(Predicate pred) {
	switch (pred) {
	case ICmpInst::ICMP_SGE: return ICmpInst::ICMP_SLE;
	case ICmpInst::ICMP_SLT: return ICmpInst::ICMP_SGT;
	case ICmpInst::ICMP_SGT: return ICmpInst::ICMP_SLT;
	case ICmpInst::ICMP_SLE: return ICmpInst::ICMP_SGE;
	default: return pred;
	}
}

bool ConstraintSet::conflict(Predicate a, Predicate b) {
	return _conflict(a, b) || _conflict(b, a);
}

bool ConstraintSet::entails(Predicate a, Predicate b) {
	if (a == b) return true;
	switch (a) {
	case ICmpInst::ICMP_EQ: return b == ICmpInst::ICMP_SGE || b == ICmpInst::ICMP_SLE;
	case ICmpInst::ICMP_SLT: return b == ICmpInst::ICMP_SLE;
	case ICmpInst::ICMP_SGT: return b == ICmpInst::ICMP_SGE;
	default: return false;
	}
}

bool ConstraintSet::_conflict(Predicate a, Predicate b) {
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
		cerr << "unsupported comparison" << endl;
		exit(1);
	}
}

Predicate ConstraintSet::join(Predicate a, Predicate b) {
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

void ConstraintSet::add(Value *lhs, Value *rhs, Predicate pred) {
	if (isBottom) return;

	addSingle(lhs, rhs, pred);
	addSingle(rhs, lhs, swappedPredicate(pred));
}

void ConstraintSet::join(const ConstraintSet &cs) {
	if (isBottom) {
		isBottom = false;
		predicates = cs.predicates;
		return;
	}

	if (cs.isBottom)
		return;

	PredicateMap newPredicates;

	// perform OR
	for (auto &pred : cs.predicates) {
		auto &lhs = pred.first.first;
		auto &rhs = pred.first.second;

		auto a = get(lhs, rhs);
		auto b = pred.second;
		Predicate c;

		if (conflict(a, b)) continue;
		else if (a == b) c = a;
		else if (entails(a, b)) c = b;
		else if (entails(b, a)) c = a;
		else
			// a must be bad predicate
			continue;

		newPredicates[pred.first] = c;
	}

	predicates = newPredicates;
}

void ConstraintSet::removeInstruction(Instruction *instr, Value *replacement) {
	struct Entry {
		pair<Value *, Value *> key;
		CmpInst::Predicate pred;
	};

	vector<Entry> pairs;
	for (auto &p : predicates)
		if (p.first.first == instr || p.first.second == instr) {
			Entry e;
			e.key = p.first;
			e.pred = p.second;
			pairs.push_back(e);
		}

	for (auto &p : pairs) {
		predicates.erase(p.key);

		if (replacement) {
			if (p.key.first == instr) p.key.first = replacement;
			if (p.key.second == instr) p.key.second = replacement;
			addSingle(p.key.first, p.key.second, p.pred);
		}
	}
}

Predicate ConstraintSet::get(Value *lhs, Value *rhs) {
	if (lhs == rhs) return ICmpInst::ICMP_EQ;
	auto it = predicates.find(make_pair(lhs, rhs));
	if (it == predicates.end()) return CmpInst::BAD_ICMP_PREDICATE;
	return it->second;
}

void ConstraintSet::set(Value *lhs, Value *rhs, Predicate pred) {
	predicates[make_pair(lhs, rhs)] = pred;
}

void ConstraintSet::addSingle(Value *lhs, Value *rhs, Predicate pred) {
	// @todo maybe use unordered_pairs with asymmetric predicates?
	// @todo also calculate entailments

	auto prevPred = get(lhs, rhs);
	if (conflict(prevPred, pred)) {
		isBottom = true;
		return;
	}

	set(lhs, rhs, join(prevPred, pred));

	auto cmp = dyn_cast<CmpInst>(lhs);
	auto cint = dyn_cast<ConstantInt>(rhs);

	// @todo also consider PHINodes
	if (cmp && cint) {
		auto subPred = cmp->getPredicate();
		bool inv = false;

		switch (pred) {
		case CmpInst::ICMP_EQ: inv = cint->isZero(); break;
		case CmpInst::ICMP_NE: inv = !cint->isZero(); break;
		default:
			cerr << "comparison comparison not supported" << endl;
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

BlockDomain::BlockDomain() {
	if (debug_mode)
		cerr << "bd created" << endl;
}

void BlockDomain::propagateConstraints(map<BasicBlock *, BlockDomain> &blocks) {
	cs = ConstraintSet();
	cs.isBottom = !isEntry;

	for (auto &edge : edges) {
		ConstraintSet edgeCS = blocks[edge.first].cs;

		auto &c = edge.second;
		if (c.cond) {
			if (auto cmp = dyn_cast<ICmpInst>(c.cond)) {
				auto pred = cmp->getPredicate();
				if (!c.condV) pred = ConstraintSet::oppositePredicate(pred);

				edgeCS.add(
					cmp->getOperand(0),
					cmp->getOperand(1),
					pred
				);
			} else {
				edgeCS.add(
					c.cond,
					ConstantInt::get(c.cond->getType(), 0),
					c.condV ? ICmpInst::ICMP_NE : ICmpInst::ICMP_EQ
				);
			}
		}

		cs.join(edgeCS);
	}
}

bool BlockDomain::isDominatedBy(BasicBlock *block) {
	return dominators.find(block) != dominators.end();
}

bool BlockDomain::replaceBlock(BasicBlock *block, BasicBlock *replacement) {
	dominators.erase(block);
	if (edges.find(block) == edges.end()) return false;
	edges[replacement] = edges[block];
	edges.erase(block);
	return true;
}

bool BlockDomain::removeEdge(BasicBlock *origin) {
	dominators.erase(origin);
	if (edges.find(origin) == edges.end()) return false;
	edges.erase(origin);
	return true;
}

bool BlockDomain::operator==(const BlockDomain &other) {
	return other.isEntry == isEntry && other.reachable == reachable;
}

void OptimizerPass::iterate(Function &func) {
	// @todo @important isDirty all the things!
	// update reachability

	for (auto &b : blocks)
		b.second.reachable = b.second.isEntry;

	bool updated = true;
	while (updated) {
		updated = false;
		for (auto &b : blocks) {
			auto &bd = b.second;
			if (bd.reachable) continue;

			for (auto &edge : bd.edges) {
				if (!blocks[edge.first].reachable) continue;
				if (!edge.second.cond ||
					getGlobalVD(edge.second.cond).contains(edge.second.condV) ||
					getGlobalVD(edge.second.cond).isBottom) {
					bd.reachable = true;
					updated = true;
					break;
				}
			}
		}
	}

	// update domains for all variables
	for (auto &block : func.getBasicBlockList())
		for (auto &inst : block.getInstList())
			hasChanged = hasChanged || trackValue(&inst, &block);
}

void OptimizerPass::fixPHINodes(Function &func) {
	for (auto &block : func.getBasicBlockList()) {
		vector<PHINode *> phiNodes;
		for (auto &inst : block.getInstList())
			if (auto phi = dyn_cast<PHINode>(&inst)) phiNodes.push_back(phi);

		for (auto &phi : phiNodes) {
			debug_print("analysing phi", phi);

			bool hasBecomeEmpty = phi->getNumOperands() == 0;
			for (unsigned i = 0; i < phi->getNumOperands();)
				if (!blocks[phi->getIncomingBlock(i)].reachable) {
					if (phi->getNumOperands() == 1) {
						// can't remove last one, PHINodes assert at least one operand
						hasBecomeEmpty = true;
						break;
					} else phi->removeIncomingValue(i);
				} else ++i;

			if (hasBecomeEmpty) {
				// parent is not reachable, this instruction will be removed anyway
				continue;
			}

			/**
			 * Check if the PHI node is singular, i. e. can be replaced by a simpler
			 * expression. The way we determine this is by checking if there is an
			 * operand that generalizes all other operands. An operand V of the
			 * PHI node generalizes all others if for each other operand V' either
			 * i) V and V' point to the same llvm value OR
			 * ii) V and V' are equal in the constraint set of the predecessor block for
			 *     V'
			 * If we find such an operand V, then we can replace the PHI node with
			 * the expression that V points to.
			 *
			 * @note While point i) might be obvious, point ii) might be a bit confusing
			 * at first. I introduced this in order to be able to remove unnecessary auxilary
			 * PHI nodes generated by code like `if (x == 10) x = 10;`.
			 */

			bool isSingular = true;
			Value *v = nullptr;
			for (unsigned j = 0; j < phi->getNumOperands(); ++j) {
				v = phi->getIncomingValue(j);
				isSingular = true;

				for (unsigned i = 0; i < phi->getNumOperands(); ++i) {
					Value *inV = phi->getIncomingValue(i);
					BasicBlock *inB = phi->getIncomingBlock(i);

					/* @todo wtf was I thinking when I wrote this?
					if (auto phi2 = dyn_cast<PHINode>(inV)) {
						if (phi2->getParent() == phi->getParent()) {
							// we're refering a phi node from the same block,
							// we can therefore directly get the value
							inV = phi2->getIncomingValueForBlock(inB);
							assert(inV);
							phi->setIncomingValue(i, inV);
						}
					}*/

					// @todo check equality, not pointer equality (also commotativity)
					if (v != inV && blocks[inB].cs.get(v, inV) != ICmpInst::ICMP_EQ) isSingular = false;
				}

				if (isSingular) break;
			}

			if (isSingular) {
				debug_print("replacing singular phi", phi);
				removeInstruction(phi, v);
			}
		}
	}
}

void OptimizerPass::fixConstants(Function &func) {
	for (auto &block : func.getBasicBlockList()) {
		struct Replacement {
			Instruction *user, *usee;
			long value;
		};

		vector<Replacement> constants;
		for (auto &inst : block.getInstList()) {
			for (auto user : inst.users()) {
				auto i = dyn_cast<Instruction>(user);
				if (!i) continue;

				auto vd = getVD(&inst, i->getParent());
				if (!vd.isConstant()) continue;

				Replacement r;
				r.user = i;
				r.usee = &inst;
				r.value = vd.min;
				constants.push_back(r);
			}
		}

		for (auto &r : constants) {
			debug_print("replacing singular value", r.usee);

			Value *newValue = ConstantInt::get(r.usee->getType(), r.value);
			r.user->replaceUsesOfWith(r.usee, newValue);
		}

		if (!constants.empty()) hasChanged = true;
	}
}

void OptimizerPass::replaceBranch(BasicBlock *origin, BranchInst *branch, BranchInst *newBranch) {
	hasChanged = true;

	if (debug_mode) {
		cerr << "replacing "; branch->print(errs());
		cerr << " with "; newBranch->print(errs());
		cerr << endl;
	}

	for (auto succ : branch->successors()) blocks[succ].removeEdge(origin);

	// fix phi values
	for (auto succ : branch->successors()) {
		auto s = newBranch->successors();
		if (find(s.begin(), s.end(), succ) != s.end()) continue;

		// deleted path
		for (auto &phi : succ->phis()) phi.removeIncomingValue(origin, false);
	}

	newBranch->insertBefore(branch);
	branch->eraseFromParent();
	values.erase(branch); // @todo investigate why this is needed

	// @todo recalculate constraint set?

	Condition c; // @todo not DRY
	c.cond = newBranch->isConditional() ? newBranch->getCondition() : nullptr;
	c.condV = 1;

	blocks[newBranch->getSuccessor(0)].edges[origin] = c;
	if (newBranch->isConditional() && newBranch->getSuccessor(0) != newBranch->getSuccessor(1)) {
		c.condV = 0;
		blocks[newBranch->getSuccessor(1)].edges[origin] = c;
	}
}

void OptimizerPass::fixBranches(Function &func) { // @todo invalidates dead-code analysis?
	for (auto &block : func.getBasicBlockList()) {
		vector<BranchInst *> branches; // conditional branches
		for (auto &inst : block.getInstList())
			if (auto br = dyn_cast<BranchInst>(&inst))
				if (br->isConditional()) branches.push_back(br);

		for (auto &branch : branches) {
			if (auto c = dyn_cast<ConstantInt>(branch->getCondition())) {
				auto newBranch = BranchInst::Create(branch->getSuccessor(c->isZero() ? 1 : 0));
				replaceBranch(&block, branch, newBranch);
				dirtyDoms = true;

				// no need to remove the edge from the unreachable successor here,
				// replaceBranch does this for us.
			} else if (branch->getSuccessor(0) == branch->getSuccessor(1)) {
				auto newBranch = BranchInst::Create(branch->getSuccessor(0));
				replaceBranch(&block, branch, newBranch);
			}
		}
	}
}

void OptimizerPass::removeDeadCode(Function &func) {
	for (auto &block : func.getBasicBlockList()) {
		vector<Instruction *> dead;
		for (auto &inst : block.getInstList())
			if (getGlobalVD(&inst).isDead)
				dead.push_back(&inst);

		for (auto &d : dead)
			removeInstruction(d);
	}
}

void OptimizerPass::removeUnreachableCode(Function &func) { // @todo name simplifyCFG
	vector<BasicBlock *> bl;
	for (auto &block : func.getBasicBlockList()) bl.push_back(&block);

	for (auto &block : bl) {
		auto &bd = blocks[block];
		if (!bd.reachable) {
			removeBlock(block);
			continue;
		}

		// merge with following block

		auto isUsless = block->getInstList().size() == 1 && !bd.isEntry;

		auto branch = dyn_cast<BranchInst>(block->getTerminator());
		if (!branch || branch->isConditional()) continue; // might be a return

		auto replacement = branch->getSuccessor(0);
		if (blocks[replacement].edges.size() > 1 && !isUsless) {
			// cannot merge with block that has multiple predecessors...
			// ...unless we're a useless block
			//cerr << "cannot merge " << block->getName().str() << " into " << replacement->getName().str() << endl;
			continue;
		}

		if (block == replacement) {
			// this can happen in case of infinite loops
			// ... let's just ignore this, okay? :-)
			continue;
		}

		mergeBlock(block, replacement);
	}
}

bool OptimizerPass::mergeBlock(BasicBlock *block, BasicBlock *replacement) {
	if (debug_mode) {
		cerr << "merge block " << block->getName().str()
			<< " into " << replacement->getName().str() << endl;
	}

	// fix all phi usages
	auto &edges = blocks[block].edges;
	for (auto &v : values)
		if (auto phi = dyn_cast<PHINode>(v.first)) { // @todo not dry!!!
			if (find(phi->blocks().begin(), phi->blocks().end(), block) == phi->blocks().end())
				continue;

			auto phiValue = phi->getIncomingValueForBlock(block);
			for (const auto &edge : edges)
				if (blocks[edge.first].reachable)
					for (const auto &b : phi->blocks())
						if (b == edge.first)
							if (phi->getIncomingValueForBlock(b) != phiValue) {
								debug_print("  merge aborted because of PHI conflict: ", phi);
								return false;
							}
		}

	hasChanged = true;

	for (auto &v : values)
		if (auto phi = dyn_cast<PHINode>(v.first)) {
			debug_print("  found phi", phi);

			if (find(phi->blocks().begin(), phi->blocks().end(), block) == phi->blocks().end())
				continue;

			auto phiValue = phi->removeIncomingValue(block, false);
			for (const auto &edge : edges)
				if (blocks[edge.first].reachable) {
					bool exists = false;
					for (const auto &b : phi->blocks())
						if (b == edge.first) {
							exists = true;
							break;
						}

					if (!exists) phi->addIncoming(phiValue, edge.first);
				}

			if (phi->getNumIncomingValues() == 0)
				// this can happen if we have a block
				// A with some value v
				// B with a phi [ A, v ]
				// single edge A -> B
				phi->replaceAllUsesWith(phiValue);

			debug_print("  changed phi", phi);
		}

	auto insertionPoint = &replacement->getInstList().front();
	vector<Instruction *> instr; // @todo copy
	for (auto &i : block->getInstList()) if (!dyn_cast<TerminatorInst>(&i)) instr.push_back(&i);
	for (auto &i : instr) i->moveBefore(insertionPoint);

	// @todo merge constraintSet?

	// dominators stay the same (@todo really?)
	blocks[replacement].isEntry = blocks[block].isEntry;
	for (auto &edge : blocks[block].edges) blocks[replacement].edges.insert(edge);
	blocks[replacement].removeEdge(block);

	for (auto &b : blocks) b.second.replaceBlock(block, replacement);

	block->replaceAllUsesWith(replacement);
	removeBlock(block);

	if (blocks[replacement].isEntry) {
		// set replacement as entry
		auto func = replacement->getParent();
		func->getBasicBlockList().remove(replacement);
		func->getBasicBlockList().push_front(replacement);
	}

	return true;
}

void OptimizerPass::removeBlock(BasicBlock *block) {
	if (debug_mode) cout << "removing block " << block->getName().str() << endl;

	/*for (auto &use : block->uses()) {
		auto &bd = blocks[block];
		// only allow self-references
		use.getUser()->print(errs()); cerr << endl;
		assert(dyn_cast<Instruction>(use.getUser())->getParent() == block);
	}*/

	hasChanged = true;

	vector<Instruction *> instr; // @todo better copy method?
	for (auto &i : block->getInstList()) instr.push_back(&i);
	for (auto &i : instr) removeInstruction(i);

	for (auto &b : blocks) b.second.removeEdge(block);
	blocks.erase(block);

	block->eraseFromParent();
}

void OptimizerPass::removeInstruction(Instruction *instr, Value *replacement) {
	assert(instr != replacement);

	hasChanged = true;

	debug_print("removing instruction", instr);

	for (auto &b : blocks)
		// @todo would it suffice to only update blocks[block] here?
		b.second.cs.removeInstruction(instr, replacement);

	valueBlacklist.insert(instr);
	values.erase(instr);

	instr->replaceAllUsesWith(
		replacement ?
			replacement :
			UndefValue::get(instr->getType())
	);
	instr->eraseFromParent();
}

ValueDomain &OptimizerPass::getGlobalVD(Value *value) {
	/*if (valueBlacklist.find(value) != valueBlacklist.end()) {
		cout << "accessing deleted value " << endl;
		value->print(outs(), true);
		auto i = dyn_cast<Instruction>(value);
		i->getParent()->print(outs());
		i->getFunction()->print(outs());
		assert(false);
	}*/

	if (values.find(value) == values.end()) {
		debug_print("creating VD for ", value);
		values[value].type = value->getType();
		values[value].makeTop();
	}

	return values[value];
}

void OptimizerPass::applyConditionToDomain(Value *value, ValueDomain &vd, const Condition &cond, BasicBlock *block) {
	if (!cond.cond) return;

	ConstraintSet cs;
	cs.add( // @todo not DRY!
		cond.cond,
		ConstantInt::get(cond.cond->getType(), 0),
		cond.condV ? ICmpInst::ICMP_NE : ICmpInst::ICMP_EQ
	);

	applyConstraintSetToDomain(value, vd, cs, block);
}

void OptimizerPass::applyConstraintSetToDomain(Value *value, ValueDomain &vd, const ConstraintSet &cs, BasicBlock *block, int max_depth) {
	for (auto &c : cs.predicates) {
		if (c.first.first != value) continue;
		if (c.first.second == value) continue;

		//debug_print("compare ", c.first.second);

		auto rhsVD = getVD(c.first.second, block, max_depth);
		if (rhsVD.isTop() || rhsVD.isBottom) continue;

		switch (c.second) {
		case CmpInst::ICMP_EQ:
			vd.min = max(vd.min, rhsVD.min);
			vd.max = min(vd.max, rhsVD.max);
			break;

		case CmpInst::ICMP_SLT:
			--rhsVD.max;
		case CmpInst::ICMP_SLE:
			vd.max = min(vd.max, rhsVD.max);
			break;

		case CmpInst::ICMP_SGT:
			++rhsVD.min;
		case CmpInst::ICMP_SGE:
			vd.min = max(vd.min, rhsVD.min);
			break;

		default:
			break;
		}
	}
}

ValueDomain OptimizerPass::getVD(Value *value, BasicBlock *block, int max_depth) {
	if (auto ci = dyn_cast<ConstantInt>(value)) {
		int intVal = (int)*ci->getValue().getRawData();

		ValueDomain vd;
		vd.isBottom = false;
		vd.type = value->getType();
		vd.min = intVal;
		vd.max = intVal;
		return vd;
	}

	if (dyn_cast<Argument>(value)) {
		return ValueDomain::top(false, value->getType());
	}

	if (dyn_cast<ConstantPointerNull>(value)) {
		ValueDomain vd;
		vd.isBottom = false;
		vd.type = value->getType();
		vd.min = 0;
		vd.max = 0;
		return vd;
	}

	//debug_print("getting ", value);

	auto vd = getGlobalVD(value);
	if (max_depth > 0)
		applyConstraintSetToDomain(value, vd, blocks[block].cs, block, max_depth - 1);

	return vd;
}

void OptimizerPass::initialize(Function &func) {
	// @todo not elegant
	blocks.clear();
	values.clear();
	valueBlacklist.clear();

	// mark entry block
	auto &entry = blocks[&func.getEntryBlock()];
	entry.reachable = true;
	entry.isEntry = true;

	// find edges
	for (auto &block : func.getBasicBlockList()) {
		auto term = block.getTerminator();

		Condition c;
		c.condV = 1;

		if (auto br = dyn_cast<BranchInst>(term))
			if (br->isConditional()) c.cond = br->getCondition();

		for (auto succ : term->successors()) {
			blocks[succ].edges[&block] = c;
			c.condV = 0;
		}
	}

	// mark unreferenced blocks as unreachable
	for (auto &block : func.getBasicBlockList())
		if (blocks.find(&block) == blocks.end())
			blocks[&block].reachable = false;

	// find dominators
	findDominators();
	propagateConstraintSets();
}

void OptimizerPass::findDominators() {
	dirtyDoms = false;

	for (auto &bd : blocks) {
		auto &dom = bd.second.dominators;
		dom.clear();

		if (bd.second.isEntry) dom.insert(bd.first);
		else for (auto &bd2 : blocks) dom.insert(bd2.first);
	}

	while (true) {
		bool hasChanged = false;
		for (auto &bd : blocks) {
			if (bd.second.edges.size() == 0) continue;

			auto &dom = bd.second.dominators;
			for (auto &edge : bd.second.edges) {
				auto &pre = blocks[edge.first];
				hasChanged = hasChanged || discard_if(dom, [&](BasicBlock *b) {
					return b != bd.first && !pre.isDominatedBy(b);
				});
			}
		}

		if (!hasChanged) break;
	}

	for (int i = 0; i < 32; ++i) {
		for (auto &b : blocks) {
			auto &bd = b.second;
			if (bd.isEntry) {
				bd.heat = 1.f;
				continue;
			}

			bd.heat = 0.f;
			for (auto &pre : bd.edges) {
				float mul = pre.second.cond ?
					(pre.second.condV ? .8f : .2f)
				: 1.f;
				bd.heat += blocks[pre.first].heat * mul;
			}
		}
	}

	if (debug_mode)
		for (auto &b : blocks)
			cout << b.first->getName().str() << " has heat " << b.second.heat << endl;

	//cerr << "  ...done" << endl;
}

void OptimizerPass::propagateConstraintSets() {
	for (auto &b : buildTopology())
		blocks[b].propagateConstraints(blocks);
}

bool OptimizerPass::shouldInstantiateBlock(BasicBlock *block) {
	if (blocks[block].edges.size() < 2)
		// can't instantiate a block with less than two predecessors
		return false;

	for (auto &phi : block->phis())
		for (auto &value : phi.incoming_values())
			if (dyn_cast<ConstantInt>(value))
				return true;

	// no promising phi node found
	return false;
}

vector<BasicBlock *> OptimizerPass::getSuccessors(BasicBlock *block) {
	vector<BasicBlock *> result;
	for (const auto &b : blocks)
		for (const auto &edge : b.second.edges)
			if (edge.first == block)
				result.push_back(b.first);
	return result;
}

Value *OptimizerPass::vmapLookup(BasicBlock *block, Instruction *value, VMap &vmap) {
	string pind;
	if (debug_mode) {
		cout << ind << "lookup "; value->print(errs()); cout << " for " << block->getName().str() << endl;
		pind = ind;
		ind = ind + "  ";
	}

	if (vmap[value].find(block) == vmap[value].end()) {
		// value is not available, do some PHI magic
		auto phi = PHINode::Create(value->getType(), (unsigned)blocks[block].edges.size());
		vmap[value][block] = phi;

		for (const auto &edge : blocks[block].edges)
			phi->addIncoming(vmapLookup(edge.first, value, vmap), edge.first);

		block->getInstList().push_front(phi);

		if (debug_mode) {
			cout << "  phi case " << block->getName().str() << endl;
			phi->print(errs()); cerr << endl;
		}
	}

	if (debug_mode) ind = pind;
	return vmap[value][block];
}

void OptimizerPass::instantiateBlock(Function *func, BasicBlock *block) {
	VMap vmap;
	int i = 0;

	struct NewBlock {
		BasicBlock *block = nullptr;
		vector<Value *> phiValues;
	};

	struct SelfPHI {
		BasicBlock *newBlock, *parentBlock;
		Instruction *localInstr;
		Instruction *refInstr;
	};

	BasicBlock *selfBlock = nullptr;
	vector<SelfPHI> selfPHIs;

	vector<NewBlock> newBlocks;

	for (const auto &edge : blocks[block].edges) {
		NewBlock nb;

		auto &parent = edge.first;
		for (auto &instr : *block) {
			if (auto phi = dyn_cast<PHINode>(&instr)) {
				auto phiValue = phi->getIncomingValueForBlock(parent);
				nb.phiValues.push_back(phiValue);
			}
		}

		for (auto &nb2 : newBlocks) {
			if (nb.phiValues == nb2.phiValues) {
				cout << "  found cached new block" << endl;
				nb.block = nb2.block;
				break;
			}
		}

		auto &newBB = nb.block;
		if (!newBB) {
			// don't have a cached new BB for this set of phi values, create one
			newBB = BasicBlock::Create(block->getContext(), block->getName().str() + "-" + std::to_string(i++), func);
			for (auto &instr : *block) {
				if (auto phi = dyn_cast<PHINode>(&instr)) {
					auto phiValue = phi->getIncomingValueForBlock(parent);
					bool isSelfPHI = false;
					if (auto phiInstr = dyn_cast<Instruction>(phiValue)) {
						if (phiInstr->getParent() == block) {
							SelfPHI s;
							s.newBlock = newBB;
							s.parentBlock = parent;
							s.localInstr = &instr;
							s.refInstr = phiInstr;
							selfPHIs.push_back(s);
							isSelfPHI = true;
						}
					}

					if (!isSelfPHI) vmap[&instr][newBB] = phiValue;
				} else {
					auto clone = instr.clone();
					vmap[&instr][newBB] = clone;
					newBB->getInstList().push_back(clone);
				}
			}

			// and now register block initially
			auto &reg = blocks[newBB];
			reg.edges[parent] = edge.second;
			reg.cs = blocks[parent].cs;

			auto &cond = edge.second;
			if (cond.cond) {
				reg.cs.add( // @todo not DRY!
					cond.cond,
					ConstantInt::get(cond.cond->getType(), 0),
					cond.condV ? ICmpInst::ICMP_NE : ICmpInst::ICMP_EQ
				);
			}

			reg.dominators = blocks[parent].dominators;
			reg.dominators.insert(newBB);

			newBlocks.push_back(nb);
		} else {
			// merge information with previously created block domain
			auto &reg = blocks[newBB];
			reg.edges[parent] = edge.second;
			discard_if(reg.dominators, [&](BasicBlock *b) {
				return b != newBB && !blocks[parent].isDominatedBy(b);
			});
		}

		// cout << parent->getName().str() << endl;

		// update branch from predecessor
		if (parent == block) selfBlock = newBB;
		else parent->getTerminator()->replaceUsesOfWith(block, newBB);
	}

	if (selfBlock)
		for (auto &newBlock : newBlocks)
			newBlock.block->getTerminator()->replaceUsesOfWith(block, selfBlock);

	// fix successor phi nodes
	for (const auto &succ : getSuccessors(block)) {
		for (auto &phi : succ->phis()) {
			auto value = phi.removeIncomingValue(block);
			for (auto &newBlock : newBlocks)
				phi.addIncoming(value, newBlock.block);
		}
	}

	// fix successor edges
	for (const auto &succ : getSuccessors(block)) {
		auto prevEdge = blocks[succ].edges[block];
		for (auto &newBlock : newBlocks)
			blocks[succ].edges[newBlock.block] = prevEdge;

		// erase old edge
		blocks[succ].edges.erase(block);
	}

	// fix self PHIs
	for (auto &selfPHI : selfPHIs) {
		// value is not available, do some PHI magic
		auto phi = PHINode::Create(selfPHI.refInstr->getType(), (unsigned)blocks[selfPHI.newBlock].edges.size());
		vmap[selfPHI.localInstr][selfPHI.newBlock] = phi;

		for (const auto &edge : blocks[selfPHI.newBlock].edges)
			phi->addIncoming(vmapLookup(edge.first, selfPHI.refInstr, vmap), edge.first);

		selfPHI.newBlock->getInstList().push_front(phi);

		if (debug_mode) {
			cout << " for " << selfPHI.newBlock->getName().str() << " (self-phi)" << endl;
			phi->print(errs()); cerr << endl;
		}
	}

	// fix references to values from outside this block
	for (auto &value : vmap) {
		vector<Use *> uses;
		for (auto &use : value.first->uses()) uses.push_back(&use); // @todo copy
		for (auto &use : uses) {
			auto user = dyn_cast<Instruction>(use->getUser());
			auto vp = user->getParent();
			if (vp == block) continue; // @todo delete the block earlier?

			use->set(vmapLookup(vp, value.first, vmap));
		}
	}

	// fix all edges conditions
	for (auto &bd : blocks) {
		if (bd.first == block) continue;

		for (auto &edge : bd.second.edges) {
			if (!edge.second.cond) continue;

			auto cond = (Instruction *)edge.second.cond;
			if (vmap.find(cond) != vmap.end()) {
				debug_print("fixing edge condition", cond);
				edge.second.cond = vmapLookup(edge.first, cond, vmap);
				debug_print("  has become", edge.second.cond);
			}
		}
	}

	removeBlock(block);
}

void OptimizerPass::instantiateBlocks(Function &func) {
	vector<BasicBlock *> blocks;
	for (auto &b : func.getBasicBlockList())
		if (shouldInstantiateBlock(&b))
			blocks.push_back(&b);

	for (auto &block : blocks) {
		if (debug_mode) cout << "should instantiate " << block->getName().str() << endl;
		instantiateBlock(&func, block);
	}
}

bool instructionIsEarlier(Instruction *a, Instruction *b) {
	// @todo there might be a smarter / more efficient way to achieve this

	if (a == nullptr) return false;
	if (b == nullptr) return true;

	for (auto &i : a->getParent()->getInstList()) {
		if (&i == a) return true;
		if (&i == b) return false;
	}

	assert(false);
	return false;
}

bool OptimizerPass::reschedule(Function &func) {
	// @todo calculate heat directly before starting the rescheduling

	struct Result {
		Instruction *instr;
		BasicBlock *newBB;

		Instruction *usage;
	};

	vector<Result> results;

	for (auto &block : func.getBasicBlockList()) {
		for (auto &instr : block.getInstList()) {
			if (hasSideEffect(&instr)) continue;
			if (isa<PHINode>(&instr)) continue;

			if (auto bin = dyn_cast<BinaryOperator>(&instr)) {
				// might cause divide-by-zero, can't reschedule those
				if (bin->getOpcode() == Instruction::SDiv) continue;
				if (bin->getOpcode() == Instruction::SRem) continue;
			}

			auto instrParent = instr.getParent();

			map<BasicBlock *, Instruction *> usage;
			set<BasicBlock *> dominators;
			bool isFirst = true; // @todo not elegant

			for (auto user : instr.users()) {
				auto useInstr = dyn_cast<Instruction>(user);
				auto useBlock = useInstr->getParent();

				if (instructionIsEarlier(useInstr, usage[useBlock]))
					usage[useBlock] = useInstr;

				if (isFirst) {
					dominators = blocks[useBlock].dominators;
					isFirst = false;
				} else {
					auto a = dominators, b = blocks[useBlock].dominators;
					dominators.clear();
					set_intersection(a.begin(), a.end(), b.begin(), b.end(), std::inserter(dominators, dominators.begin()));
				}

				if (dyn_cast<PHINode>(useInstr))
					// we can't schedule an instruction into a block with a PHI node that depends on it
					// (@todo implement this)
					dominators.erase(useBlock);
			}

			for (auto &op : instr.operands()) {
				auto instrRef = dyn_cast<Instruction>(op.get());
				if (!instrRef) continue;

				auto opParent = instrRef->getParent();
				for (auto &d : blocks[opParent].dominators) {
					if (d == opParent) continue;
					dominators.erase(d);
				}
			}

			if (dominators.empty())
				// no use found
				continue;

			auto lowestHeatBB = *min_element(dominators.begin(), dominators.end(), [&](BasicBlock *a, BasicBlock *b) {
				return blocks[a].heat < blocks[b].heat;
			});

			/*
			instr.print(outs());
			for (auto &dom : dominators) {
				cout << "- " << dom->getName().str() << endl;
			}
			*/

			if (instrParent != lowestHeatBB) {
				Result result;
				result.instr = &instr;
				result.newBB = lowestHeatBB;
				result.usage = usage[result.newBB];
				results.push_back(result);
			}
		}
	}

	for (auto &result : results) {
		if (debug_mode) {
			cout << "reschedule ";
			result.instr->print(outs());
			cout << " into " << result.newBB->getName().str() << endl;
		}

		result.instr->removeFromParent();

		if (result.usage) {
			// insert before first usage
			result.instr->insertBefore(result.usage);
		} else {
			// ensure we're inserted after all dependencies
			result.instr->insertBefore(result.newBB->getTerminator());
		}
	}

	return !results.empty();
}

vector<BasicBlock *> OptimizerPass::buildTopology() {
	vector<BasicBlock *> topology;
	for (auto &b : blocks)
		topology.push_back(b.first);

	sort(topology.begin(), topology.end(), [&](BasicBlock *a, BasicBlock *b) {
		return blocks[a].dominators.size() < blocks[b].dominators.size();
	});

	return topology;
}

bool OptimizerPass::runOnFunction(Function &func) {
	initialize(func);

	if (debug_mode) {
		print(outs(), nullptr);
		func.print(errs());
	}

	int it = 0;
	while (true) {
		isAscending = it > 500; // @todo hack!

		if (debug_mode) {
			if (it > 0) cout << endl;
			cout << "iteration #" << it << endl;
		}

		hasChanged = false;

		iterate(func);

		if (!hasChanged) {
			// domain analysis has finished

			fixPHINodes(func);
			fixConstants(func);
			fixBranches(func);
			removeDeadCode(func);
			removeUnreachableCode(func);

			if (dirtyDoms) {
				findDominators();
				propagateConstraintSets();
			}
		}

		if (debug_mode && hasChanged) func.print(errs());

		if (!hasChanged) {
			if (optimizer->options.symex)
				instantiateBlocks(func);
			if (debug_mode) func.print(errs());
			if (!hasChanged)
				break;
		}

		if (++it > 1000) {
			cerr << func.getName().str() << endl;
			cerr << "aborting optimization" << endl;
			return false;
		}
	}

	if (optimizer->options.licm)
		while (reschedule(func));

	if (debug_mode) {
		cout << endl << "fixpoint:" << endl;
		print(outs(), nullptr);
	}

	return true;
}

bool OptimizerPass::hasSideEffect(Value *v) {
	return !(
		dyn_cast<Constant>(v) ||
		dyn_cast<BinaryOperator>(v) || // we can optimize this, div-by-zero is undefined behavior!
		dyn_cast<ICmpInst>(v) ||
		dyn_cast<PHINode>(v) ||
		dyn_cast<SelectInst>(v)
	);
}

bool OptimizerPass::trackValue(Value *v, BasicBlock *block) {
	auto prevVd = getGlobalVD(v);
	auto &vd = getGlobalVD(v);

	vd.isDead = true;
	if (blocks[block].reachable) {
		//v->print(errs()); cerr << " " << (hasSideEffect(v) ? "se" : "ne") << endl;
		if (hasSideEffect(v)) vd.isDead = false;
		else { // check if we're referenced
			for (auto &use : v->uses()) {
				//cerr << "used by "; use.getUser()->print(errs()); cerr << endl;
				auto &x = getGlobalVD(use.getUser());
				if (!x.isDead) {
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
	} else if (auto bc = dyn_cast<BitCastInst>(v)) {
		vd = getVD(bc->User::getOperand(0), block); // @todo
	} else if (dyn_cast<AllocaInst>(v)) {
		vd.isBottom = false;
		vd.min = 1; // @todo not exactly correct (pointers are unsigned!)
	} else if (
		dyn_cast<GetElementPtrInst>(v) ||
		dyn_cast<LoadInst>(v) ||
		dyn_cast<StoreInst>(v) ||
		dyn_cast<GlobalValue>(v) ||
		dyn_cast<CallInst>(v) ||
		dyn_cast<Constant>(v)
	) {
		vd = ValueDomain::top(false, v->getType());
	} else if (auto trunc = dyn_cast<TruncInst>(v)) {
		auto op = getVD(trunc->getOperand(0), block);

		bool overflow = false;

		vd.isBottom = op.isBottom;
		vd.min = vd.truncate(op.min, overflow);
		vd.max = vd.truncate(op.max, overflow);

		if (overflow && !vd.isConstant()) // @todo common and unprecise pattern!
			vd.makeTop();
	} else if (auto sext = dyn_cast<SExtInst>(v)) {
		auto op = getVD(sext->getOperand(0), block);

		vd.isBottom = op.isBottom;
		vd.min = op.min;
		vd.max = op.max;
	} else if (auto bin = dyn_cast<BinaryOperator>(v)) {
		auto lhs = getVD(bin->getOperand(0), block);
		auto rhs = getVD(bin->getOperand(1), block);

		if (lhs.isBottom) vd = lhs;
		else if (rhs.isBottom) vd = rhs;
		else {
			vd.isBottom = false;

			switch (bin->getOpcode()) {
			case Instruction::Add: vd = ValueDomain::add(v->getType(), lhs, rhs); break;
			case Instruction::Sub: {
				auto epred = blocks[block].cs.get(bin->getOperand(0), bin->getOperand(1));
				vd = ValueDomain::sub(v->getType(), lhs, rhs);

				switch (epred) { // @todo what about overflows?
				case CmpInst::ICMP_EQ: vd.min = vd.max = 0L; break;
				case CmpInst::ICMP_SGT: vd.min = max(vd.min, 1L); break;
				case CmpInst::ICMP_SGE: vd.min = max(vd.min, 0L); break;
				case CmpInst::ICMP_SLT: vd.max = min(vd.max, -1L); break;
				case CmpInst::ICMP_SLE: vd.max = min(vd.max, 0L); break;
				default: break;
				}

				break;
			}

			case Instruction::Mul: vd = ValueDomain::mul(v->getType(), lhs, rhs); break;
			case Instruction::SDiv: vd = ValueDomain::div(v->getType(), lhs, rhs); break;

			default:
				if (debug_mode)
					cerr << "unsupported binary operation" << endl;
				vd.makeTop();
			}
		}
	} else if (auto phi = dyn_cast<PHINode>(v)) {
		vd.isBottom = true;

		for (unsigned i = 0; i < phi->getNumIncomingValues(); ++i) {
			auto inBB = phi->getIncomingBlock(i);
			if (blocks[inBB].reachable) {
				auto inV = phi->getIncomingValue(i);
				auto inVd = getVD(inV, inBB);
				applyConditionToDomain(inV, inVd, blocks[block].edges[inBB], inBB); // @todo instead of inBB, block?
				vd = ValueDomain::join(vd, inVd);
			}
		}
	} else if (auto cmp = dyn_cast<ICmpInst>(v)) {
		auto lhs = getVD(cmp->getOperand(0), block);
		auto rhs = getVD(cmp->getOperand(1), block);

		bool vTrue, vFalse, swapOut = false;
		auto cmpPred = cmp->getPredicate();
		auto entailPred = blocks[block].cs.get(cmp->getOperand(0), cmp->getOperand(1));

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
			cerr << "unsupported comparison" << endl;
			exit(1);
		}

		if (vTrue && vFalse && entailPred == ICmpInst::BAD_ICMP_PREDICATE) {
			// got an entailing pred
			vTrue = !ConstraintSet::conflict(entailPred, cmpPred);
			vFalse = !ConstraintSet::entails(entailPred, cmpPred);
		}

		if (swapOut)
			swap(vFalse, vTrue);

		vd.isBottom = lhs.isBottom || rhs.isBottom;
		vd.min = vFalse ? 0 : 1;
		vd.max = vTrue ? 1 : 0;
	} else {
		debug_print("unsupported instruction", v);
		vd = ValueDomain::top(false, v->getType());
	}

	if (isAscending)
		vd = ValueDomain::join(prevVd, vd, vd.isDead);

	if (prevVd == vd)
		return false;

	if (!prevVd.isBottom && !vd.isBottom && !prevVd.isTop() && !vd.isTop() && !vd.isDead) {
		if (debug_mode)
			cout << "widening " << prevVd << " / " << vd << endl;

		bool maxOk = vd.max == prevVd.max || (vd.max < prevVd.max && !isAscending);
		bool minOk = vd.min == prevVd.min || (vd.min > prevVd.min && !isAscending);
		long min = vd.min, max = vd.max;

		vd.makeTop();

		if (minOk) vd.min = min;
		if (maxOk) vd.max = max;

		if (prevVd == vd)
			return false;
	}

	if (debug_mode) {
		cout << "change: value ";
		if (v->hasName()) cout << string(v->getName());
		else v->print(errs());
		cout << " (" << prevVd << " -> " << vd << ")" << endl;
	}
	return true;
}

void OptimizerPass::print(raw_ostream &, const Module *) const {
	// @todo @bug output to specified stream, not cout!

	for (auto &bd : blocks)
		cout << "block: " << string(bd.first->getName()) << bd.second;

	for (auto &v : values) {
		auto &vd = v.second;
		cout << "value ";
		v.first->print(outs());
		cout << ": " << vd << endl;
		//v.first->print(errs());
	}
}

char OptimizerPass::ID = 0;

}

