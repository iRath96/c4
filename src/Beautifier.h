#ifndef Beautifier_h
#define Beautifier_h

#include "AST.h"
#include <functional>
#include <iostream>

/**
 * Reads external AST from its source and outputs these in a human-readable format
 * to stdout.
 */
class Beautifier : public ast::Visitor, public streams::Sink<ast::Ptr<ast::External>> {
protected:
	std::string indent = "";
	bool isFirst = true;
public:
	bool lispMode = true;

	Beautifier(Source<ast::Ptr<ast::External>> *source) : Sink<ast::Ptr<ast::External>>(source) {}

	virtual bool next(void *) {
		ast::Ptr<ast::External> result;
		if (this->source->next(&result)) {
			if (isFirst) isFirst = false;
			else std::cout << std::endl;

			inspect(*result);
			return true;
		} else
			return false;
	}

protected:
	void inspect(ast::Node &node) { node.accept(*this); }
	void inspect(const char *str) { std::cout << str; }

	template<typename T>
	void inspect(ast::Ptr<T> &ptr) {
		if (ptr.get()) inspect(*ptr);
	}

	template<typename T>
	void join(ast::Vector<T> &vector, std::string delimiter, std::string suffix = "") {
		bool first = true;

		for (auto &child : vector) {
			if (first) first = false;
			else std::cout << delimiter;

			inspect(child);
		}

		if (!vector.empty()) std::cout << suffix;
	}

	template<typename T>
	void separate_lines(ast::Vector<T> &vector, bool do_indent = true) {
		std::string prev_indent = indent;
		if (do_indent) indent += "\t";

		for (auto &child : vector) {
			std::cout << std::endl << indent;
			inspect(child);
		}

		indent = prev_indent;
		std::cout << std::endl << indent;
	}

	bool isDeclaratorEmpty(ast::Declarator &decl) const;

	static bool inline_if(ast::Statement *node, bool afterElse = false);
	void inline_inspect(ast::Statement *node, bool suffix = false, bool afterElse = false);

	virtual void visit(ast::CaseLabel &node);
	virtual void visit(ast::DefaultLabel &);
	virtual void visit(ast::IdentifierLabel &node);
	virtual void visit(ast::Identifier &node);
	virtual void visit(ast::NamedTypeSpecifier &node);
	virtual void visit(ast::ContinueStatement &node);
	virtual void visit(ast::CompoundStatement &node);
	virtual void visit(ast::DeclaratorPointer &);
	virtual void visit(ast::DeclaratorParameterList &node);
	virtual void visit(ast::Declarator &node);
	virtual void visit(ast::Declaration &node);
	virtual void visit(ast::GlobalVariable &node);
	virtual void visit(ast::Function &node);
	virtual void visit(ast::ParameterDeclaration &node);
	virtual void visit(ast::IdentifierExpression &node);
	virtual void visit(ast::Constant &node);
	virtual void visit(ast::StringLiteral &node);
	virtual void visit(ast::CastExpression &node);
	virtual void visit(ast::UnaryExpression &node);
	virtual void visit(ast::BinaryExpression &node);
	virtual void visit(ast::ConditionalExpression &node);
	virtual void visit(ast::ExpressionList &node);
	virtual void visit(ast::CallExpression &node);
	virtual void visit(ast::SubscriptExpression &node);
	virtual void visit(ast::MemberExpression &node);
	virtual void visit(ast::PostExpression &node);
	virtual void visit(ast::ExpressionStatement &node);
	virtual void visit(ast::SizeofExpressionUnary &node);
	virtual void visit(ast::TypeName &node);
	virtual void visit(ast::ComposedTypeSpecifier &node);
	virtual void visit(ast::SizeofExpressionTypeName &node);
	virtual void visit(ast::IterationStatement &node);
	virtual void visit(ast::SelectionStatement &node);
	virtual void visit(ast::GotoStatement &node);
	virtual void visit(ast::ReturnStatement &node);
};

#endif /* Beautifier_h */
