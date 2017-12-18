#ifndef Compiler_h
#define Compiler_h

#include "AST.h"
#include <iostream>

using namespace ast;
class Compiler : public Visitor<void>, public Stream<Ptr<ExternalDeclaration>, Ptr<ExternalDeclaration>> {
protected:
	void inspect(Node &node) {
		node.accept(*this);
	}

	template<typename T>
	void inspect(ast::Ptr<T> &node) {
		if (node.get()) node->accept(*this);
	}

public:
	Compiler(Source<Ptr<ast::ExternalDeclaration>> *source)
	: Stream<Ptr<ExternalDeclaration>, Ptr<ExternalDeclaration>>(source) {
	}

	virtual bool next(ast::Ptr<ast::ExternalDeclaration> *result) {
		if (this->source->next(result)) {
			inspect(*result);
			return true;
		} else {
			return false;
		}
	}

	virtual void visit(CaseLabel &node) {
	}

	virtual void visit(DefaultLabel &node) {
	}

	virtual void visit(IdentifierLabel &node) {
	}

	void visit(Statement &node) {
	}

	virtual void visit(GotoStatement &node) {
	}

	virtual void visit(ContinueStatement &node) {
	}

	virtual void visit(Identifier &) {}

	virtual void visitBlockItems(CompoundStatement &node) {
	}

	virtual void visit(CompoundStatement &node) {
	}

	virtual void visit(DeclaratorPointer &) {}
	virtual void visit(DeclaratorParameterList &) {}
	virtual void visit(Declarator &) {}

	virtual void visit(TypeName &node) {
	}

	virtual void visit(Declaration &node) {
	}

	virtual void visit(ExternalDeclarationVariable &node) {
	}

	virtual void visit(ExternalDeclarationFunction &node) {
		//std::cout << "function" << std::endl;
	}

	virtual void visit(ParameterDeclaration &node) {
	}

#pragma mark - Expressions

	virtual void visit(ConstantExpression &node) {
	}

	virtual void visit(CastExpression &node) {
	}

	virtual void visit(UnaryExpression &node) {
	}

	virtual void visit(BinaryExpression &node) {
	}

	virtual void visit(ConditionalExpression &node) {
	}

	virtual void visit(ExpressionList &node) {
	}

	virtual void visit(CallExpression &node) {
	}

	virtual void visit(SubscriptExpression &node) {
	}

	virtual void visit(MemberExpression &node) {
	}

	virtual void visit(PostExpression &node) {
	}

	virtual void visit(ExpressionStatement &node) {
	}

	virtual void visit(SizeofExpressionTypeName &node) {
	}

	virtual void visit(SizeofExpressionUnary &node) {
	}

	virtual void visit(ComposedTypeSpecifier &) {}
	virtual void visit(NamedTypeSpecifier &) {}

	virtual void visit(IterationStatement &node) {
	}

	virtual void visit(SelectionStatement &node) {
	}

	virtual void visit(ReturnStatement &node) {
	}
};

#endif
