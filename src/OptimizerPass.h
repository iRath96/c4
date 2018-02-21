#ifndef OptimizerPass_h
#define OptimizerPass_h

#include "Optimizer.h"

#include <set>
#include <map>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/Pass.h>
#include <llvm/IR/Instructions.h>
#pragma GCC diagnostic pop


namespace llvm {
	class Module;
	class Function;
	class Value;
	class BasicBlock;
}


namespace optimizer {

using Predicate = llvm::CmpInst::Predicate;

struct OptimizerPass : public llvm::FunctionPass {
	const Optimizer *optimizer;

	static char ID;
	OptimizerPass(const Optimizer *optimizer)
	: FunctionPass(ID), optimizer(optimizer) {}

	struct Condition {
		llvm::Value *cond = nullptr;
		bool condV;
	};

	struct ValueDomain {
		llvm::Type *type = nullptr;

		bool isBottom = true;
		bool isDead = false;
		long min = 0, max = 0;

		llvm::Value *memSize = nullptr; // @todo @important for malloc (+memIndex)

		static ValueDomain join(ValueDomain &a, ValueDomain &b, bool isDead = false);
		static ValueDomain top(bool isDead, llvm::Type *type);

		static ValueDomain add(llvm::Type *type, ValueDomain &lhs, ValueDomain &rhs);
		static ValueDomain sub(llvm::Type *type, ValueDomain &lhs, ValueDomain &rhs);
		static ValueDomain mul(llvm::Type *type, ValueDomain &lhs, ValueDomain &rhs);
		static ValueDomain div(llvm::Type *type, ValueDomain &lhs, ValueDomain &rhs);
		static ValueDomain mod(llvm::Type *type, ValueDomain &lhs, ValueDomain &rhs);

		bool contains(int v) const;
		bool isConstant() const;
		bool isTop() const;

		void makeTop();

		long addOverflow(long a, long b, bool &overflow) const;
		long subOverflow(long a, long b, bool &overflow) const;
		long mulOverflow(long a, long b, bool &overflow) const;
		long truncate(long v, bool &overflow) const;

		int bitWidth() const;
		void fullRange(long &min, long &max) const;

		bool operator==(const ValueDomain &other) const;
	};

	struct ConstraintSet {
		std::map<std::pair<llvm::Value *, llvm::Value *>, Predicate> predicates;

		bool isBottom = false; // bottom <=> can never become true
		bool isTop() const { return !isBottom && predicates.empty(); }

		static Predicate oppositePredicate(Predicate pred);
		static Predicate swappedPredicate(Predicate pred);

		static bool conflict(Predicate a, Predicate b); // x <a> y <=> !(x <b> y)
		static bool entails(Predicate a, Predicate b); // x <a> y => x <b> y

	protected:
		static bool _conflict(Predicate a, Predicate b);

	public:
		static Predicate join(Predicate a, Predicate b); // a "and" b

		void add(llvm::Value *lhs, llvm::Value *rhs, Predicate pred);
		void join(const ConstraintSet &cs);
		void removeInstruction(llvm::Instruction *instr, llvm::Value *replacement = nullptr);

		Predicate get(llvm::Value *lhs, llvm::Value *rhs);

	protected:
		void set(llvm::Value *lhs, llvm::Value *rhs, Predicate pred);
		void addSingle(llvm::Value *lhs, llvm::Value *rhs, Predicate pred);
	};

	struct BlockDomain {
		BlockDomain();

		float heat = 0.f;
		bool isEntry = false, reachable = false;

		std::set<llvm::BasicBlock *> dominators;
		std::map<llvm::BasicBlock *, Condition> edges;
		ConstraintSet cs;

		void propagateConstraints(std::map<llvm::BasicBlock *, BlockDomain> &blocks);
		bool isDominatedBy(llvm::BasicBlock *block);
		bool replaceBlock(llvm::BasicBlock *block, llvm::BasicBlock *replacement);
		bool removeEdge(llvm::BasicBlock *origin);

		bool operator==(const BlockDomain &other);
	};

	std::map<llvm::BasicBlock *, BlockDomain> blocks;
	std::map<llvm::Value *, ValueDomain> values;

	std::set<llvm::Value *> valueBlacklist;

	bool hasChanged, isAscending;

	bool hasSideEffect(llvm::Value *v);
	bool trackValue(llvm::Value *v, llvm::BasicBlock *block);

	void iterate(llvm::Function &func);
	void fixPHINodes(llvm::Function &func);
	void fixConstants(llvm::Function &func);
	void replaceBranch(llvm::BasicBlock *origin, llvm::BranchInst *branch, llvm::BranchInst *newBranch);
	void fixBranches(llvm::Function &func); // @todo invalidates dead-code analysis?
	void removeDeadCode(llvm::Function &func);
	void removeUnreachableCode(llvm::Function &func); // @todo name simplifyCFG
	bool mergeBlock(llvm::BasicBlock *block, llvm::BasicBlock *replacement);

	void removeBlock(llvm::BasicBlock *block);
	void removeInstruction(llvm::Instruction *instr, llvm::Value *replacement = nullptr);

	void applyConditionToDomain(llvm::Value *value, ValueDomain &vd, const Condition &cond, llvm::BasicBlock *block);
	void applyConstraintSetToDomain(llvm::Value *value, ValueDomain &vd, const ConstraintSet &cs, llvm::BasicBlock *block, int max_depth = 4);

	ValueDomain &getGlobalVD(llvm::Value *value);
	ValueDomain getVD(llvm::Value *value, llvm::BasicBlock *block, int max_depth = 4);

	void initialize(llvm::Function &func);

	void findDominators(); // fixpoint approach
	void propagateConstraintSets();

	bool shouldInstantiateBlock(llvm::BasicBlock *block);

	std::vector<llvm::BasicBlock *> getSuccessors(llvm::BasicBlock *block);

	// @todo outsource this
	typedef std::map<llvm::Instruction *, std::map<llvm::BasicBlock *, llvm::Value *>> VMap;
	std::string ind = "";
	llvm::Value *vmapLookup(llvm::BasicBlock *block, llvm::Instruction *value, VMap &vmap);

	void instantiateBlock(llvm::Function *func, llvm::BasicBlock *block);
	void instantiateBlocks(llvm::Function &func);

	bool reschedule(llvm::Function &func);
	std::vector<llvm::BasicBlock *> buildTopology();

	bool runOnFunction(llvm::Function &func) override;
	void print(llvm::raw_ostream &out, const llvm::Module *module) const override;
};

}

#endif /* OptimizerPass_h */
