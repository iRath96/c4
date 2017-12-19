#ifndef Beautifier_h
#define Beautifier_h

#include "AST.h"
#include <functional>

using namespace ast;

class Beautifier : public Visitor, public Sink<ast::Ptr<ast::External>> {
protected:
	std::string indent = "";
	bool isFirst = true;

	void inspect(Node &node) { node.accept(*this); }
	void inspect(const char *str) { std::cout << str; }

	template<typename T>
	void inspect(Ptr<T> &ptr) {
		if (ptr.get()) inspect(*ptr);
	}

	template<typename T>
	void join(Vector<T> &vector, std::string delimiter, std::string suffix = "") {
		bool first = true;

		for (auto &child : vector) {
			if (first) first = false;
			else std::cout << delimiter;

			inspect(child);
		}

		if (!vector.empty()) std::cout << suffix;
	}

	template<typename T>
	void separate_lines(Vector<T> &vector, bool do_indent = true) {
		std::string prev_indent = indent;
		if (do_indent) indent += "\t";

		for (auto &child : vector) {
			std::cout << std::endl << indent;
			inspect(child);
		}

		indent = prev_indent;
		std::cout << std::endl << indent;
	}

public:
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
	virtual void visit(CaseLabel &node) {
		std::cout << "case ";
		inspect(*node.expression);
		std::cout << ":";
	}

	virtual void visit(DefaultLabel &) { std::cout << "default:"; }
	virtual void visit(IdentifierLabel &node) { std::cout << node.id << ":"; }
	virtual void visit(Identifier &node) { std::cout << node.id; }
	virtual void visit(NamedTypeSpecifier &node) { std::cout << node.id; }
	virtual void visit(ContinueStatement &node) {
		std::cout << (node.keyword == lexer::Token::Keyword::CONTINUE ? "continue;" : "break;");
	}

	virtual void visit(CompoundStatement &node) {
		std::cout << "{";

		std::string prev_indent = indent;
		indent += "\t";

		for (auto &child : node.items) {
			std::cout << std::endl;

			Statement *stmt;
			if ((stmt = dynamic_cast<Statement *>(child.get())))
				join(stmt->labels, "\n", "\n");

			std::cout << indent;
			inspect(child);
		}

		indent = prev_indent;
		std::cout << std::endl << indent << "}";
	}

	virtual void visit(DeclaratorPointer &) { std::cout << "*"; }

	virtual void visit(DeclaratorParameterList &node) { // @todo variadic
		std::cout << "(";
		if (node.parameters.empty()) std::cout << "void";
		else join(node.parameters, ", ");
		std::cout << ")";
	}

	virtual void visit(Declarator &node) {
		for (auto &mod : node.modifiers) {
			std::cout << "(";
			if (dynamic_cast<DeclaratorPointer *>(mod.get())) // @todo DeclaratorPrefix class?
				inspect(mod);
		}

		if (!node.isAbstract())
			std::cout << node.name;

		for (auto it = node.modifiers.rbegin(); it != node.modifiers.rend(); ++it) {
			if (!dynamic_cast<DeclaratorPointer *>(it->get()))
				inspect(*it);
			std::cout << ")";
		}

		if (node.initializer.get()) {
			std::cout << " = ";
			inspect(node.initializer);
		}
	}

	virtual void visit(Declaration &node) {
		join(node.specifiers, " ", node.declarators.empty() ? "" : " ");
		join(node.declarators, ", ");
		std::cout << ";";
	}

	virtual void visit(GlobalVariable &node) {
		visit(node.declaration);
		std::cout << std::endl;
	}

	virtual void visit(Function &node) {
		join(node.declaration.specifiers, " ", " ");
		join(node.declaration.declarators, ", ");

		separate_lines(node.declarations, true);

		inspect(node.body);
		std::cout << std::endl;
	}

	bool isDeclaratorEmpty(ast::Declarator &decl) const {
		return decl.modifiers.empty() && decl.isAbstract();
	}

	virtual void visit(ParameterDeclaration &node) {
		join(node.specifiers, " ", isDeclaratorEmpty(node.declarator) ? "" : " ");
		inspect(node.declarator);
	}

	virtual void visit(IdentifierExpression &node) { std::cout << node.text; }
	virtual void visit(Constant &node) { std::cout << node.text; }
	virtual void visit(StringLiteral &node) { std::cout << node.text; }

	virtual void visit(CastExpression &node) {
		std::cout << "((";
		inspect(node.type);
		std::cout << ")";
		inspect(node.expression);
		std::cout << ")";
	}

	virtual void visit(UnaryExpression &node) {
		std::cout << "(";
		std::cout << operator_name(node.op);
		inspect(node.operand);
		std::cout << ")";
	}

	virtual void visit(BinaryExpression &node) {
		std::cout << "(";
		inspect(node.lhs);
		std::cout << " " << operator_name(node.op) << " ";
		inspect(node.rhs);
		std::cout << ")";
	}

	virtual void visit(ConditionalExpression &node) {
		std::cout << "(";
		inspect(node.condition);
		std::cout << " ? ";
		inspect(node.when_true);
		std::cout << " : ";
		inspect(node.when_false);
		std::cout << ")";
	}

	virtual void visit(ExpressionList &node) { join(node.children, ", "); }

	virtual void visit(CallExpression &node) {
		std::cout << "(";
		inspect(node.function);
		std::cout << "(";
		join(node.arguments, ", ");
		std::cout << ")";
		std::cout << ")";
	}

	virtual void visit(SubscriptExpression &node) {
		std::cout << "(";
		inspect(node.base);
		std::cout << "[";
		inspect(node.subscript);
		std::cout << "]";
		std::cout << ")";
	}

	virtual void visit(MemberExpression &node) {
		std::cout << "(";
		inspect(node.base);
		std::cout << (node.dereference ? "->" : ".") << node.id;
		std::cout << ")";
	}

	virtual void visit(PostExpression &node) {
		std::cout << "(";
		inspect(node.base);
		std::cout << operator_name(node.op);
		std::cout << ")";
	}

	virtual void visit(ExpressionStatement &node) {
		inspect(node.expressions);
		std::cout << ";";
	}

	virtual void visit(SizeofExpressionUnary &node) {
		std::cout << "(sizeof ";
		inspect(node.expression);
		std::cout << ")";
	}

	virtual void visit(TypeName &node) {
		join(node.specifiers, " ", isDeclaratorEmpty(node.declarator) ? "" : " ");
		inspect(node.declarator);
	}

	virtual void visit(ComposedTypeSpecifier &node) {
		std::cout << "struct";
		if (node.isNamed()) std::cout << " " << node.name;

		if (!node.declarations.empty()) {
			std::cout << std::endl << indent << "{";
			separate_lines(node.declarations);
			std::cout << "}";
		}
	}

	virtual void visit(SizeofExpressionTypeName &node) {
		std::cout << "(sizeof(";
		inspect(node.type);
		std::cout << "))";
	}

	virtual void visit(IterationStatement &node) {
		std::cout << "while (";
		inspect(node.condition);
		std::cout << ")";

		inline_inspect(node.body.get());
	}

	static bool inline_if(Statement *node, bool afterElse = false) {
		return !(dynamic_cast<ExpressionStatement *>(node)
			|| dynamic_cast<ReturnStatement *>(node)
			|| dynamic_cast<JumpStatement *>(node)
			|| dynamic_cast<IterationStatement *>(node)
			|| (!afterElse && dynamic_cast<SelectionStatement *>(node))
		);
	}

	void inline_inspect(Statement *node, bool suffix = false, bool afterElse = false) {
		std::string pi = indent;

		if (inline_if(node, afterElse)) {
			if (node->labels.empty())
				std::cout << " ";
			else { // has labels, can't inline this easily.
				std::cout << std::endl;
				join(node->labels, "\n", "\n"); // @todo not DRY
				std::cout << indent;
			}
		} else {
			indent += "\t";

			std::cout << std::endl;
			join(node->labels, "\n", "\n");
			std::cout << indent;
		}

		inspect(*node);
		indent = pi;

		if (suffix) {
			if (inline_if(node, afterElse)) std::cout << " ";
			else std::cout << std::endl << indent;
		}
	}

	virtual void visit(SelectionStatement &node) {
		std::cout << "if (";
		inspect(node.condition);
		std::cout << ")";

		inline_inspect(node.when_true.get(), node.when_false.get() != NULL);

		if (node.when_false.get()) {
			std::cout << "else";
			inline_inspect(node.when_false.get(), false, true);
		}
	}

	virtual void visit(GotoStatement &node) {
		std::cout << "goto " << node.target << ";";
	}

	virtual void visit(ReturnStatement &node) {
		std::cout << "return";
		if (!node.expressions.children.empty()) std::cout << " ";
		inspect(node.expressions);
		std::cout << ";";
	}
};

#endif /* Beautifier_h */
