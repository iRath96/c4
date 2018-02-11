#include "Beautifier.h"

using namespace ast;
using namespace std;

void Beautifier::visit(CaseLabel &node) {
	std::cout << "case ";
	inspect(*node.expression);
	std::cout << ":";
}

void Beautifier::visit(DefaultLabel &) { std::cout << "default:"; }
void Beautifier::visit(IdentifierLabel &node) { std::cout << node.id << ":"; }
void Beautifier::visit(Identifier &node) { std::cout << node.id; }
void Beautifier::visit(NamedTypeSpecifier &node) { std::cout << node.id; }
void Beautifier::visit(ContinueStatement &node) {
	std::cout << (node.keyword == lexer::Token::Keyword::CONTINUE ? "continue;" : "break;");
}

void Beautifier::visit(CompoundStatement &node) {
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

void Beautifier::visit(DeclaratorPointer &) { std::cout << "*"; }

void Beautifier::visit(DeclaratorParameterList &node) { // @todo variadic
	std::cout << "(";
	if (node.removedVoid) std::cout << "void";
	else join(node.parameters, ", ");
	std::cout << ")";
}

void Beautifier::visit(Declarator &node) {
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

void Beautifier::visit(Declaration &node) {
	join(node.specifiers, " ", node.declarators.empty() ? "" : " ");
	join(node.declarators, ", ");
	std::cout << ";";
}

void Beautifier::visit(GlobalVariable &node) {
	visit(node.declaration);
	std::cout << std::endl;
}

void Beautifier::visit(Function &node) {
	join(node.declaration.specifiers, " ", " ");
	join(node.declaration.declarators, ", ");

	separate_lines(node.declarations, true);

	inspect(node.body);
	std::cout << std::endl;
}

bool Beautifier::isDeclaratorEmpty(ast::Declarator &decl) const {
	return decl.modifiers.empty() && decl.isAbstract();
}

void Beautifier::visit(ParameterDeclaration &node) {
	join(node.specifiers, " ", isDeclaratorEmpty(node.declarator) ? "" : " ");
	inspect(node.declarator);
}

void Beautifier::visit(IdentifierExpression &node) { std::cout << node.text; }
void Beautifier::visit(Constant &node) { std::cout << node.text; }
void Beautifier::visit(StringLiteral &node) { std::cout << node.text; }

void Beautifier::visit(CastExpression &node) {
	std::cout << "((";
	inspect(node.type);
	std::cout << ")";
	inspect(node.expression);
	std::cout << ")";
}

void Beautifier::visit(UnaryExpression &node) {
	std::cout << "(";
	std::cout << lexer::Token::operatorName(node.op);
	inspect(node.operand);
	std::cout << ")";
}

void Beautifier::visit(BinaryExpression &node) {
	std::cout << "(";
	inspect(node.lhs);
	std::cout << " " << lexer::Token::operatorName(node.op) << " ";
	inspect(node.rhs);
	std::cout << ")";
}

void Beautifier::visit(ConditionalExpression &node) {
	std::cout << "(";
	inspect(node.condition);
	std::cout << " ? ";
	inspect(node.when_true);
	std::cout << " : ";
	inspect(node.when_false);
	std::cout << ")";
}

void Beautifier::visit(ExpressionList &node) { join(node.children, ", "); }

void Beautifier::visit(CallExpression &node) {
	std::cout << "(";
	inspect(node.function);
	std::cout << "(";
	join(node.arguments, ", ");
	std::cout << ")";
	std::cout << ")";
}

void Beautifier::visit(SubscriptExpression &node) {
	std::cout << "(";
	inspect(node.base);
	std::cout << "[";
	inspect(node.subscript);
	std::cout << "]";
	std::cout << ")";
}

void Beautifier::visit(MemberExpression &node) {
	std::cout << "(";
	inspect(node.base);
	std::cout << (node.dereference ? "->" : ".") << node.id;
	std::cout << ")";
}

void Beautifier::visit(PostExpression &node) {
	std::cout << "(";
	inspect(node.base);
	std::cout << lexer::Token::operatorName(node.op);
	std::cout << ")";
}

void Beautifier::visit(ExpressionStatement &node) {
	inspect(node.expressions);
	std::cout << ";";
}

void Beautifier::visit(SizeofExpressionUnary &node) {
	std::cout << "(sizeof ";
	inspect(node.expression);
	std::cout << ")";
}

void Beautifier::visit(TypeName &node) {
	join(node.specifiers, " ", isDeclaratorEmpty(node.declarator) ? "" : " ");
	inspect(node.declarator);
}

void Beautifier::visit(ComposedTypeSpecifier &node) {
	std::cout << "struct";
	if (node.isNamed()) std::cout << " " << node.name;

	if (!node.declarations.empty()) {
		std::cout << std::endl << indent << "{";
		separate_lines(node.declarations);
		std::cout << "}";
	}
}

void Beautifier::visit(SizeofExpressionTypeName &node) {
	std::cout << "(sizeof(";
	inspect(node.type);
	std::cout << "))";
}

void Beautifier::visit(IterationStatement &node) {
	std::cout << "while (";
	inspect(node.condition);
	std::cout << ")";

	inline_inspect(node.body.get());
}

bool Beautifier::inline_if(Statement *node, bool afterElse) {
	return !(dynamic_cast<ExpressionStatement *>(node)
		|| dynamic_cast<ReturnStatement *>(node)
		|| dynamic_cast<JumpStatement *>(node)
		|| dynamic_cast<IterationStatement *>(node)
		|| (!afterElse && dynamic_cast<SelectionStatement *>(node))
	);
}

void Beautifier::inline_inspect(Statement *node, bool suffix, bool afterElse) {
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

void Beautifier::visit(SelectionStatement &node) {
	std::cout << "if (";
	inspect(node.condition);
	std::cout << ")";

	inline_inspect(node.when_true.get(), node.when_false.get() != NULL);

	if (node.when_false.get()) {
		std::cout << "else";
		inline_inspect(node.when_false.get(), false, true);
	}
}

void Beautifier::visit(GotoStatement &node) {
	std::cout << "goto " << node.target << ";";
}

void Beautifier::visit(ReturnStatement &node) {
	std::cout << "return";
	if (!node.expressions.children.empty()) std::cout << " ";
	inspect(node.expressions);
	std::cout << ";";
}
