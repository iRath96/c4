#ifndef AST_h
#define AST_h

#include "Lexer.h"

namespace ast {

#pragma mark - Type definitions

template<typename T>
using Ptr = std::shared_ptr<T>;

template<typename T>
using Vector = std::vector<T>;

template<typename T>
using PtrVector = Vector<Ptr<T>>;

#pragma mark - Visitor

template<typename T>
struct Visitor {
	// Nodes
	virtual T visit(struct Identifier &) = 0;
	virtual T visit(struct TypeName &) = 0;

	// Labels
	virtual T visit(struct CaseLabel &) = 0;
	virtual T visit(struct DefaultLabel &) = 0;
	virtual T visit(struct IdentifierLabel &) = 0;

	// Declarators
	virtual T visit(struct DeclaratorParameterList &) = 0;
	virtual T visit(struct DeclaratorPointer &) = 0;
	virtual T visit(struct Declarator &) = 0;

	// Expressions
	virtual T visit(struct ConstantExpression &) = 0;
	virtual T visit(struct CastExpression &) = 0;
	virtual T visit(struct UnaryExpression &) = 0;
	virtual T visit(struct BinaryExpression &) = 0;
	virtual T visit(struct ConditionalExpression &) = 0;
	virtual T visit(struct ExpressionList &) = 0;
	virtual T visit(struct CallExpression &) = 0;
	virtual T visit(struct SubscriptExpression &) = 0;
	virtual T visit(struct MemberExpression &) = 0;
	virtual T visit(struct PostExpression &) = 0;
	virtual T visit(struct SizeofExpressionUnary &) = 0;
	virtual T visit(struct SizeofExpressionTypeName &) = 0;

	// Statements
	virtual T visit(struct CompoundStatement &) = 0;
	virtual T visit(struct IterationStatement &) = 0;
	virtual T visit(struct ExpressionStatement &) = 0;
	virtual T visit(struct SelectionStatement &) = 0;
	virtual T visit(struct GotoStatement &) = 0;
	virtual T visit(struct ContinueStatement &) = 0;
	virtual T visit(struct ReturnStatement &) = 0;

	// Declarations
	virtual T visit(struct Declaration &) = 0;
	virtual T visit(struct ParameterDeclaration &) = 0;
	virtual T visit(struct ExternalDeclarationVariable &) = 0;
	virtual T visit(struct ExternalDeclarationFunction &) = 0;

	// Types
	virtual T visit(struct NamedTypeSpecifier &) = 0;
	virtual T visit(struct ComposedTypeSpecifier &) = 0;
};

#pragma mark - Base classes

#define ACCEPT virtual void accept(Visitor<void> &v) { v.visit(*this); }

struct Node {
	lexer::TextPosition pos;
	virtual void accept(Visitor<void> &) = 0;
};

struct Label : Node {};
struct BlockItem : Node {};
struct Expression : Node {};

struct Statement : BlockItem {
	PtrVector<Label> labels;
};

#pragma mark - Nodes

struct Identifier : Node {
	std::string id;
	ACCEPT
};

struct TypeSpecifier : Node {};

#pragma mark - Labels

struct CaseLabel : Label {
	Ptr<Expression> expression;
	ACCEPT
};

struct DefaultLabel : Label {
	ACCEPT
};

struct IdentifierLabel : Label {
	std::string id;
	ACCEPT
};

#pragma mark - Declarators

struct DeclaratorModifier : Node {};

struct DeclaratorParameterList : DeclaratorModifier {
	ast::Vector<ast::ParameterDeclaration> parameters;
	ACCEPT
};

struct DeclaratorPointer : DeclaratorModifier {
	ACCEPT
};

struct Declarator : Node {
	PtrVector<DeclaratorModifier> modifiers;
	Ptr<Expression> initializer;

	std::string name;
	bool isAbstract() const { return name.empty(); }

	ACCEPT
};

struct TypeName : Node {
	PtrVector<TypeSpecifier> specifiers;
	Declarator declarator;
	ACCEPT
};

#pragma mark - Expressions

struct ConstantExpression : Expression {
	std::string text;
	bool isIdentifier;
	ACCEPT
};

struct CastExpression : Expression {
	TypeName type;
	Ptr<Expression> expression;
	ACCEPT
};

struct UnaryExpression : Expression {
	Ptr<Expression> operand;
	lexer::Token::Punctuator op;
	ACCEPT
};

struct BinaryExpression : Expression {
public:
	Ptr<Expression> lhs, rhs;
	lexer::Token::Punctuator op;
	ACCEPT
};

struct ConditionalExpression : Expression {
	Ptr<Expression> condition, when_true, when_false;
	ACCEPT
};

struct ExpressionList : Expression {
	PtrVector<Expression> children;
	ACCEPT
};

struct CallExpression : Expression {
	Ptr<Expression> function;
	PtrVector<Expression> arguments;
	ACCEPT
};

struct SubscriptExpression : Expression {
	Ptr<Expression> base;
	ast::ExpressionList subscript;
	ACCEPT
};

struct MemberExpression : Expression {
	bool dereference; // false for '.', true for '->'

	Ptr<Expression> base;
	std::string id;

	ACCEPT
};

struct PostExpression : Expression {
	Ptr<Expression> base;
	lexer::Token::Punctuator op;
	ACCEPT
};

struct SizeofExpression : Expression {};

struct SizeofExpressionUnary : SizeofExpression {
	Ptr<Expression> expression;
	ACCEPT
};

struct SizeofExpressionTypeName : SizeofExpression { // @todo wtf?
	TypeName type;
	ACCEPT
};

#pragma mark - Statements

struct CompoundStatement : Statement {
	PtrVector<BlockItem> items;
	ACCEPT
};

struct ExpressionStatement : Statement {
	ExpressionList expressions;
	ACCEPT
};

struct IterationStatement : Statement {
	ExpressionList condition;
	Ptr<Statement> body;
	ACCEPT
};

struct SelectionStatement : Statement {
	ExpressionList condition;
	Ptr<Statement> when_true, when_false;
	ACCEPT
};

struct JumpStatement : Statement {};

struct GotoStatement : JumpStatement {
	std::string target;
	ACCEPT
};

struct ContinueStatement : JumpStatement {
	lexer::Token::Keyword keyword; // BREAK or CONTINUE
	ACCEPT
};

struct ReturnStatement : JumpStatement {
	ExpressionList expressions;
	ACCEPT
};

#pragma mark - Declarations

struct Declaration : BlockItem {
	PtrVector<TypeSpecifier> specifiers;
	Vector<Declarator> declarators;
	ACCEPT
};

struct ParameterDeclaration : Node {
	PtrVector<TypeSpecifier> specifiers;
	Declarator declarator;
	ACCEPT
};

struct ExternalDeclaration : Declaration {};

struct ExternalDeclarationVariable : ExternalDeclaration {
	ACCEPT
};

struct ExternalDeclarationFunction : ExternalDeclaration {
	Vector<Declaration> declarations;
	CompoundStatement body;
	ACCEPT
};

#pragma mark - Types

struct NamedTypeSpecifier : TypeSpecifier {
	std::string id;
	lexer::Token::Keyword keyword;
	ACCEPT
};

struct ComposedTypeSpecifier : TypeSpecifier {
	std::string name;

	lexer::Token::Keyword kind; // STRUCT or UNION
	Vector<Declaration> declarations;

	bool isQualified() const { return !declarations.empty(); }
	bool isNamed() const { return !name.empty(); }

	ACCEPT
};

#undef ACCEPT

}

#endif /* AST_h */
