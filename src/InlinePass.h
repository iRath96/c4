#ifndef InlinePass_h
#define InlinePass_h

#include "Optimizer.h"

#include <set>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/Pass.h>
#include <llvm/IR/LegacyPassManager.h>
#pragma GCC diagnostic pop


namespace llvm {
	class CallInst;
	class Module;
	class Function;
}


namespace optimizer {

struct InlinePass : public llvm::ModulePass {
	const Optimizer *optimizer;

	static char ID;
	InlinePass(const Optimizer *optimizer)
	: ModulePass(ID), optimizer(optimizer) {}

	void processCall(llvm::CallInst *call, std::set<llvm::Function *> &dirtyFns);
	bool runOnModule(llvm::Module &module) override;
};

}

#endif /* InlinePass_h */
