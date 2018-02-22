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

/**
 * Does some basic inlining by trying to integrate the control flow of called functions into the caller function.
 */
struct InlinePass : public llvm::ModulePass {
	const Optimizer *optimizer;

	static char ID;
	InlinePass(const Optimizer *optimizer)
	: ModulePass(ID), optimizer(optimizer) {}

	/**
	 * Tries to inline the given call and adds the caller function to dirtyFns if the inlining
	 * succeeds.
	 * @param call The call that is to be inlined (caller and callee will be derived from this)
	 * @param [out] dirtyFns Set of functions that have been modified during inlining
	 */
	void processCall(llvm::CallInst *call, std::set<llvm::Function *> &dirtyFns);
	bool runOnModule(llvm::Module &module) override;
};

}

#endif /* InlinePass_h */
