#ifndef Compiler_h
#define Compiler_h

#include "AST.h"
#include "Analyzer.h"

#include <iostream>
#include <map>
#include <vector>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DataLayout.h>
#pragma GCC diagnostic pop

namespace llvm {
	class Value;
	class PHINode;
	class Type;
	class Module;
};

class Compiler;
struct CompilerResult { // @todo put into Compiler
	Compiler *compiler;
	std::vector<llvm::GlobalValue *> values;
	bool shouldExecute;

	CompilerResult(Compiler *compiler = nullptr) : compiler(compiler) {}
};

class Compiler : public ast::Visitor, public streams::Stream<ast::Ptr<ast::External>, CompilerResult> {
protected:
	void inspect(ast::Node &node) {
		node.accept(*this);
	}

	template<typename T>
	void inspect(ast::Ptr<T> &node) { // @todo not DRY
		if (node.get()) node->accept(*this);
	}

	llvm::LLVMContext ctx;
	llvm::IRBuilder<> builder, allocaBuilder;
	llvm::Module *mod;
	llvm::DataLayout dataLayout;

public:
	std::unique_ptr<llvm::Module> modPtr;

protected:
	struct Loop {
		llvm::BasicBlock *header, *body, *end;
	};

	struct {
		bool needed = false;
		bool prevLN = false; // needed for ExpressionList
		llvm::BasicBlock *tBB = nullptr, *fBB = nullptr;
	} logical;

	bool shouldLoad;
	llvm::Value *value;
	llvm::Function *func;

	CompilerResult cres = CompilerResult(this);

	std::map<const ast::DeclarationRef *, llvm::Value *> values;
	std::map<const ast::Type *, llvm::Type *> types;

	Loop loop;
	std::map<std::string, llvm::BasicBlock *> labels;
	std::map<std::string, std::vector<llvm::BranchInst *>> labelRefs;

public:
	Compiler(streams::Source<ast::Ptr<ast::External>> *source, std::string moduleName);
	virtual bool next(CompilerResult *result);

protected:
	llvm::Type *createType(const ast::Type *type);
	llvm::Value *getValue(ast::Expression &expr, bool load = true, bool logical = false);
	llvm::Value *matchType(llvm::Value *value, llvm::Type *type);

	llvm::Value *performAdd(llvm::Value *lhs, llvm::Value *rhs, std::string name = "add");
	llvm::Value *performSub(llvm::Value *lhs, llvm::Value *rhs, std::string name = "sub");

	void createLabels(const ast::PtrVector<ast::Label> &labels);
	void createDeadBlock();

	llvm::PHINode *createLogicalPHI(llvm::BasicBlock *&post);
	void createLogicalAnd(ast::BinaryExpression &node);
	void createLogicalOr(ast::BinaryExpression &node);
	void createLogicalNot(ast::UnaryExpression &node);

	virtual void visit(ast::REPLStatement &node);
	virtual void visit(ast::CompoundStatement &node);
	virtual void visit(ast::IterationStatement &node);
	virtual void visit(ast::SelectionStatement &node);
	virtual void visit(ast::ReturnStatement &node);
	virtual void visit(ast::GotoStatement &node);
	virtual void visit(ast::ContinueStatement &node);
	virtual void visit(ast::ExpressionStatement &node);

	void declaration(ast::Declaration &node, bool isGlobal);
	void visit(ast::Declaration &node);
	virtual void visit(ast::GlobalVariable &node);
	virtual void visit(ast::Function &node);

	virtual void visit(ast::IdentifierExpression &node);
	virtual void visit(ast::Constant &node);
	virtual void visit(ast::StringLiteral &node);
	virtual void visit(ast::UnaryExpression &node);
	virtual void visit(ast::BinaryExpression &node);
	virtual void visit(ast::ConditionalExpression &);
	virtual void visit(ast::ExpressionList &node);
	virtual void visit(ast::CallExpression &node);
	virtual void visit(ast::CastExpression &node);
	virtual void visit(ast::SubscriptExpression &node);
	virtual void visit(ast::MemberExpression &node);
	virtual void visit(ast::PostExpression &node);
	virtual void visit(ast::SizeofExpressionTypeName &node);
	virtual void visit(ast::SizeofExpressionUnary &node);
};

#endif
