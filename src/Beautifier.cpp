#include "Beautifier.h"


using namespace ast;
using namespace std;


namespace utils {

void Beautifier::visit(CaseLabel &node) {
	cout << "case ";
	inspect(*node.expression);
	cout << ":";
}

void Beautifier::visit(DefaultLabel &) { cout << "default:"; }
void Beautifier::visit(IdentifierLabel &node) { cout << node.id << ":"; }
void Beautifier::visit(Identifier &node) { cout << node.id; }
void Beautifier::visit(NamedTypeSpecifier &node) { cout << node.id; }
void Beautifier::visit(ContinueStatement &node) {
	cout << (node.keyword == lexer::Token::Keyword::CONTINUE ? "continue;" : "break;");
}

void Beautifier::visit(CompoundStatement &node) {
	cout << "{";

	string prev_indent = indent;
	indent += "\t";

	for (auto &child : node.items) {
		cout << endl;

		Statement *stmt;
		if ((stmt = dynamic_cast<Statement *>(child.get())))
			join(stmt->labels, "\n", "\n");

		cout << indent;
		inspect(child);
	}

	indent = prev_indent;
	cout << endl << indent << "}";
}

void Beautifier::visit(DeclaratorPointer &) { cout << "*"; }

void Beautifier::visit(DeclaratorParameterList &node) { // @todo variadic
	cout << "(";
	if (node.removedVoid) cout << "void";
	else join(node.parameters, ", ");
	if (node.isVariadic) cout << ", ...";
	cout << ")";
}

void Beautifier::visit(Declarator &node) {
	for (auto &mod : node.modifiers) {
		if (lispMode) cout << "(";
		if (dynamic_cast<DeclaratorPointer *>(mod.get())) // @todo DeclaratorPrefix class?
			inspect(mod);
	}

	if (!node.isAbstract())
		cout << node.name;

	for (auto it = node.modifiers.rbegin(); it != node.modifiers.rend(); ++it) {
		if (!dynamic_cast<DeclaratorPointer *>(it->get()))
			inspect(*it);
		if (lispMode) cout << ")";
	}

	if (node.initializer.get()) {
		cout << " = ";
		inspect(node.initializer);
	}
}

void Beautifier::visit(Declaration &node) {
	join(node.specifiers, " ", node.declarators.empty() ? "" : " ");
	join(node.declarators, ", ");
	cout << ";";
}

void Beautifier::visit(GlobalVariable &node) {
	visit(node.declaration);
	cout << endl;
}

void Beautifier::visit(Function &node) {
	join(node.declaration.specifiers, " ", " ");
	join(node.declaration.declarators, ", ");

	separate_lines(node.declarations, true);

	inspect(node.body);
	cout << endl;
}

bool Beautifier::isDeclaratorEmpty(ast::Declarator &decl) const {
	return decl.modifiers.empty() && decl.isAbstract();
}

void Beautifier::visit(ParameterDeclaration &node) {
	join(node.specifiers, " ", isDeclaratorEmpty(node.declarator) ? "" : " ");
	inspect(node.declarator);
}

void Beautifier::visit(IdentifierExpression &node) { cout << node.text; }
void Beautifier::visit(Constant &node) { cout << node.text; }
void Beautifier::visit(StringLiteral &node) { cout << node.text; }

void Beautifier::visit(CastExpression &node) {
	if (lispMode) cout << "(";
	cout << "(";
	inspect(node.type);
	cout << ")";
	inspect(node.expression);
	if (lispMode) cout << ")";
}

void Beautifier::visit(UnaryExpression &node) {
	if (lispMode) cout << "(";
	cout << lexer::Token::operatorName(node.op);
	inspect(node.operand);
	if (lispMode) cout << ")";
}

void Beautifier::visit(BinaryExpression &node) {
	if (lispMode) cout << "(";
	inspect(node.lhs);
	cout << " " << lexer::Token::operatorName(node.op) << " ";
	inspect(node.rhs);
	if (lispMode) cout << ")";
}

void Beautifier::visit(ConditionalExpression &node) {
	if (lispMode) cout << "(";
	inspect(node.condition);
	cout << " ? ";
	inspect(node.when_true);
	cout << " : ";
	inspect(node.when_false);
	if (lispMode) cout << ")";
}

void Beautifier::visit(ExpressionList &node) { join(node.children, ", "); }

void Beautifier::visit(CallExpression &node) {
	if (lispMode) cout << "(";
	inspect(node.function);
	cout << "(";
	join(node.arguments, ", ");
	cout << ")";
	if (lispMode) cout << ")";
}

void Beautifier::visit(SubscriptExpression &node) {
	if (lispMode) cout << "(";
	inspect(node.base);
	cout << "[";
	inspect(node.subscript);
	cout << "]";
	if (lispMode) cout << ")";
}

void Beautifier::visit(MemberExpression &node) {
	if (lispMode) cout << "(";
	inspect(node.base);
	cout << (node.dereference ? "->" : ".") << node.id;
	if (lispMode) cout << ")";
}

void Beautifier::visit(PostExpression &node) {
	if (lispMode) cout << "(";
	inspect(node.base);
	cout << lexer::Token::operatorName(node.op);
	if (lispMode) cout << ")";
}

void Beautifier::visit(ExpressionStatement &node) {
	inspect(node.expressions);
	cout << ";";
}

void Beautifier::visit(SizeofExpressionUnary &node) {
	if (lispMode) cout << "(";
	cout << "sizeof ";
	inspect(node.expression);
	if (lispMode) cout << ")";
}

void Beautifier::visit(TypeName &node) {
	join(node.specifiers, " ", isDeclaratorEmpty(node.declarator) ? "" : " ");
	inspect(node.declarator);
}

void Beautifier::visit(ComposedTypeSpecifier &node) {
	cout << "struct";
	if (node.isNamed()) cout << " " << node.name;

	if (!node.declarations.empty()) {
		cout << endl << indent << "{";
		separate_lines(node.declarations);
		cout << "}";
	}
}

void Beautifier::visit(SizeofExpressionTypeName &node) {
	if (lispMode) cout << "(";
	cout << "sizeof(";
	inspect(node.type);
	cout << ")";
	if (lispMode) cout << ")";
}

void Beautifier::visit(IterationStatement &node) {
	cout << "while (";
	inspect(node.condition);
	cout << ")";

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
	string pi = indent;

	if (inline_if(node, afterElse)) {
		if (node->labels.empty())
			cout << " ";
		else { // has labels, can't inline this easily.
			cout << endl;
			join(node->labels, "\n", "\n"); // @todo not DRY
			cout << indent;
		}
	} else {
		indent += "\t";

		cout << endl;
		join(node->labels, "\n", "\n");
		cout << indent;
	}

	inspect(*node);
	indent = pi;

	if (suffix) {
		if (inline_if(node, afterElse)) cout << " ";
		else cout << endl << indent;
	}
}

void Beautifier::visit(SelectionStatement &node) {
	cout << "if (";
	inspect(node.condition);
	cout << ")";

	inline_inspect(node.when_true.get(), node.when_false.get() != NULL);

	if (node.when_false.get()) {
		cout << "else";
		inline_inspect(node.when_false.get(), false, true);
	}
}

void Beautifier::visit(GotoStatement &node) {
	cout << "goto " << node.target << ";";
}

void Beautifier::visit(ReturnStatement &node) {
	cout << "return";
	if (!node.expressions.children.empty()) cout << " ";
	inspect(node.expressions);
	cout << ";";
}

}
