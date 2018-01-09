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
	class Type;
	class Module;
};

using namespace ast;

class Compiler;
struct CompilerResult { // @todo put into Compiler
	Compiler *compiler;
	std::vector<llvm::GlobalValue *> values;
	bool shouldExecute;

	CompilerResult(Compiler *compiler = nullptr) : compiler(compiler) {}
};

class Compiler : public Visitor, public Stream<Ptr<External>, CompilerResult> {
protected:
	void inspect(Node &node) {
		node.accept(*this);
	}

	template<typename T>
	void inspect(Ptr<T> &node) { // @todo not DRY
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

	bool shouldLoad;
	llvm::Value *value;
	llvm::Function *func;

	CompilerResult cres = CompilerResult(this);

	std::map<const DeclarationRef *, llvm::Value *> values;
	std::map<const Type *, llvm::Type *> types;

	Loop loop;
	std::map<std::string, llvm::BasicBlock *> labels;
	std::map<std::string, std::vector<llvm::BranchInst *>> labelRefs;

public:
	Compiler(Source<Ptr<ast::External>> *source, std::string moduleName);
	virtual bool next(CompilerResult *result);

protected:
	llvm::Type *createType(const Type *type);
	llvm::Value *getValue(Expression &expr, bool load = true);
	llvm::Value *matchType(llvm::Value *value, llvm::Type *type);
	llvm::Value *testZero(llvm::Value *v);

	void createLabels(const PtrVector<Label> &labels);
	void createDeadBlock();

	void createLogicalAnd(BinaryExpression &node);
	void createLogicalOr(BinaryExpression &node);

	virtual void visit(REPLStatement &node);
	virtual void visit(CompoundStatement &node);
	virtual void visit(IterationStatement &node);
	virtual void visit(SelectionStatement &node);
	virtual void visit(ReturnStatement &node);
	virtual void visit(GotoStatement &node);
	virtual void visit(ContinueStatement &node);
	virtual void visit(ExpressionStatement &node);

	void declaration(Declaration &node, bool isGlobal);
	void visit(Declaration &node);
	virtual void visit(GlobalVariable &node);
	virtual void visit(Function &node);

	virtual void visit(IdentifierExpression &node);
	virtual void visit(Constant &node);
	virtual void visit(StringLiteral &node);
	virtual void visit(UnaryExpression &node);
	virtual void visit(BinaryExpression &node);
	virtual void visit(ConditionalExpression &);
	virtual void visit(ExpressionList &node);
	virtual void visit(CallExpression &node);
	virtual void visit(CastExpression &node);
	virtual void visit(SubscriptExpression &node);
	virtual void visit(MemberExpression &node);
	virtual void visit(PostExpression &node);
	virtual void visit(SizeofExpressionTypeName &node);
	virtual void visit(SizeofExpressionUnary &node);
};

#endif
