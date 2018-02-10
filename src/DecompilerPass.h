#ifndef Decompiler_h
#define Decompiler_h

#include <map>
#include <vector>

#include "AST.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include <llvm/Pass.h>
#include <llvm/IR/Constants.h>
#pragma GCC diagnostic pop


namespace llvm {
	class BasicBlock;
	class Value;
	class PHINode;
	class Instruction;
}

struct DecompilerPass : public llvm::FunctionPass {
	static char ID;
	DecompilerPass() : FunctionPass(ID) {}

	void getAnalysisUsage(llvm::AnalysisUsage &info) const override;
	bool runOnFunction(llvm::Function &func) override;

	std::map<llvm::Instruction *, bool> ignoreInst;
	std::map<llvm::BasicBlock *, std::string> labels;
	std::map<llvm::BasicBlock *, std::vector<llvm::PHINode *>> phiRefs;
	std::map<llvm::BasicBlock *, ast::Ptr<ast::CompoundStatement>> bmap;
	std::map<llvm::Value *, ast::Ptr<ast::Expression>> vmap;

	ast::Ptr<ast::Expression> resolve(llvm::Value *value);

	void decompileBlock(llvm::BasicBlock *block);
	llvm::BasicBlock *findFirstDominator(llvm::BasicBlock *block);
	void resolvePHIRefs(llvm::BasicBlock *block);
};

#endif /* Decompiler_h */
