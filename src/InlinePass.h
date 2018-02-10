#ifndef InlinePass_h
#define InlinePass_h

#include <set>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include <llvm/Pass.h>
#pragma GCC diagnostic pop


namespace llvm {
	class CallInst;
	class Module;
	class Function;
}

struct InlinePass : public llvm::ModulePass {
	static char ID;
	InlinePass() : ModulePass(ID) {}

	void processCall(llvm::CallInst *call, std::set<llvm::Function *> &dirtyFns);
	bool runOnModule(llvm::Module &module) override;
};

#endif /* InlinePass_h */
