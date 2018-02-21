#ifndef Decompiler_h
#define Decompiler_h

#include "AST.h"
#include "Optimizer.h"

#include <map>
#include <vector>
#include <set>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/Pass.h>
#include <llvm/IR/Constants.h>
#include <llvm/Support/raw_ostream.h>
#pragma GCC diagnostic pop


namespace llvm {
	class BasicBlock;
	class Value;
	class PHINode;
	class Instruction;
}


namespace optimizer {

struct DecompilerPass : public llvm::FunctionPass {
	const Optimizer *optimizer;

	static char ID;
	DecompilerPass(const Optimizer *optimizer)
	: FunctionPass(ID), optimizer(optimizer) {}

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

	/**
	 * @return Whether the provided body is a compound statement that has become empty
	 */
	bool fixGotos(ast::Statement *body, std::set<std::string> &refs, ast::IdentifierLabel *follow = nullptr);
	void fixLabels(ast::Statement *body, const std::set<std::string> &refs);

	void unwrapCompoundStatement(ast::Ptr<ast::Statement> &stmt);
	void negateExpression(ast::ExpressionList &expr);
	void decompileType(
		llvm::Type *type,
		ast::PtrVector<ast::TypeSpecifier> &specifiers,
		ast::PtrVector<ast::DeclaratorModifier> &modifiers
	);
};

}

#endif /* Decompiler_h */
