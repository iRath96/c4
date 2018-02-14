#ifndef Decompiler_h
#define Decompiler_h

#include <map>
#include <vector>
#include <set>

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
	std::map<llvm::BasicBlock *, std::vector<llvm::PHINode *>> phiRefs;
	std::map<llvm::Value *, ast::Ptr<ast::Expression>> vmap;

	std::map<llvm::Value *, std::string> names;
	std::map<std::string, llvm::Value *> namesReverse; // @todo not elegant

	int nameCounter = 0;

	std::string resolveName(llvm::Value *value);

	ast::Ptr<ast::Expression> resolve(llvm::Value *value);

	llvm::BasicBlock *findFirstDominator(llvm::BasicBlock &block);

	bool isLoop(llvm::BasicBlock *header, llvm::BasicBlock *body);

	void bindBlock(llvm::BasicBlock &block, ast::CompoundStatement &compound, std::set<llvm::BasicBlock *> &join);
	void decompileBlock(llvm::BasicBlock &block, ast::CompoundStatement &compound);
	void resolvePHIRefs(llvm::BasicBlock &block, ast::CompoundStatement &compound);

	void fixGotos(ast::Statement *body, std::set<std::string> &refs, ast::IdentifierLabel *follow = nullptr);
	void fixLabels(ast::Statement *body, const std::set<std::string> &refs);
};

#endif /* Decompiler_h */
