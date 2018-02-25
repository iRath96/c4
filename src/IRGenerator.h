#ifndef IRGenerator_h
#define IRGenerator_h

#include "AST.h"
#include "Analyzer.h"

#include <iostream>
#include <map>
#include <vector>
#include <stack>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DIBuilder.h>
#pragma GCC diagnostic pop


namespace llvm {
	class Value;
	class Instruction;
	class PHINode;
	class Type;
	class Module;
};


namespace compiler {

class IRGenerator;
struct IRFragment { // @todo put into IRGenerator
	IRGenerator *compiler;
	std::vector<llvm::GlobalValue *> values;
	bool shouldExecute;

	IRFragment(IRGenerator *compiler = nullptr) : compiler(compiler) {}
};

class IRGenerator : public ast::Visitor, public streams::Stream<ast::Ptr<ast::External>, IRFragment> {
protected:
	void inspect(ast::Node &node);

	llvm::LLVMContext ctx;
	llvm::Module *mod;
	llvm::IRBuilder<> builder, allocaBuilder;
	llvm::DIBuilder diBuilder;
	llvm::DataLayout dataLayout;

	llvm::DIFile *diFile;
	std::stack<llvm::DIScope *> diStack;

public:
	std::unique_ptr<llvm::Module> modPtr;
	bool emitDebug;

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

	IRFragment cres = IRFragment(this);

	std::map<const DeclarationRef *, llvm::Value *> values;
	std::map<const Type *, llvm::Type *> types;

	Loop loop;
	std::map<std::string, llvm::BasicBlock *> labels;
	std::map<std::string, std::vector<llvm::BranchInst *>> labelRefs;

public:
	IRGenerator(streams::Source<ast::Ptr<ast::External>> *source, std::string moduleName, bool emitDebug);
	virtual bool next(IRFragment *result);

protected:
	llvm::Type *createType(const Type *type);
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

	void createSizeof(ast::Node &node);
	virtual void visit(ast::SizeofExpressionTypeName &node);
	virtual void visit(ast::SizeofExpressionUnary &node);
};

}

#endif
