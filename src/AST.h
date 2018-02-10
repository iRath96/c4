#ifndef AST_h
#define AST_h

#include <memory>
#include <string>
#include <vector>

#include "common.h"
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

struct Visitor {
	// Nodes
	virtual void visit(struct Identifier &) {}
	virtual void visit(struct TypeName &) {}

	// Labels
	virtual void visit(struct CaseLabel &) {}
	virtual void visit(struct DefaultLabel &) {}
	virtual void visit(struct IdentifierLabel &) {}

	// Declarators
	virtual void visit(struct DeclaratorParameterList &) {}
	virtual void visit(struct DeclaratorPointer &) {}
	virtual void visit(struct Declarator &) {}

	// Expressions
	virtual void visit(struct IdentifierExpression &) {}
	virtual void visit(struct Constant &) {}
	virtual void visit(struct StringLiteral &) {}
	
	virtual void visit(struct CastExpression &) {}
	virtual void visit(struct UnaryExpression &) {}
	virtual void visit(struct BinaryExpression &) {}
	virtual void visit(struct ConditionalExpression &) {}
	virtual void visit(struct ExpressionList &) {}
	virtual void visit(struct CallExpression &) {}
	virtual void visit(struct SubscriptExpression &) {}
	virtual void visit(struct MemberExpression &) {}
	virtual void visit(struct PostExpression &) {}
	virtual void visit(struct SizeofExpressionUnary &) {}
	virtual void visit(struct SizeofExpressionTypeName &) {}

	// Statements
	virtual void visit(struct CompoundStatement &) {}
	virtual void visit(struct IterationStatement &) {}
	virtual void visit(struct ExpressionStatement &) {}
	virtual void visit(struct SelectionStatement &) {}
	virtual void visit(struct GotoStatement &) {}
	virtual void visit(struct ContinueStatement &) {}
	virtual void visit(struct ReturnStatement &) {}

	// Declarations
	virtual void visit(struct Declaration &) {}
	virtual void visit(struct ParameterDeclaration &) {}
	virtual void visit(struct GlobalVariable &) {}
	virtual void visit(struct Function &) {}
	virtual void visit(struct REPLStatement &) {}

	// Types
	virtual void visit(struct NamedTypeSpecifier &) {}
	virtual void visit(struct ComposedTypeSpecifier &) {}
};

#pragma mark - Base classes

#define ACCEPT virtual void accept(Visitor &v) { v.visit(*this); }

/**
 * Information attached to an AST node, typically as a result of a form of analysis.
 * @see Analyzer
 */
struct Annotation {
	virtual ~Annotation() {}
};

/**
 * A node in the AST tree.
 */
struct Node {
	Ptr<Annotation> annotation;
	void annotate(Annotation *a) { annotation.reset(a); }
	void annotate(Ptr<Annotation> a) { annotation = a; }

	common::TextPosition pos;
	virtual void accept(Visitor &) = 0;
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
	Vector<ParameterDeclaration> parameters;
	bool isVariadic = false;
	bool removedVoid = false;
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

struct IdentifierExpression : Expression {
	IdentifierExpression() {}
	IdentifierExpression(const std::string &text) : text(text) {}

	std::string text;
	ACCEPT
};

struct Constant : Expression {
	Constant() {}
	Constant(const std::string &text, int value, bool isChar)
	: text(text), value(value), isChar(isChar) {}

	std::string text;
	int value;
	bool isChar;
	ACCEPT
};

struct StringLiteral : Expression {
	StringLiteral() {}
	StringLiteral(const std::string &text) : text(text), value(text) {}
	
	std::string text;
	std::string value;
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
	bool isExternal; // @todo storage class

	PtrVector<TypeSpecifier> specifiers;
	Vector<Declarator> declarators;
	ACCEPT
};

struct ParameterDeclaration : Node {
	PtrVector<TypeSpecifier> specifiers;
	Declarator declarator;
	ACCEPT
};

struct External : Node {};
struct ExternalDeclaration : External {
	Declaration declaration; // @todo not elegant
};

struct GlobalVariable : ExternalDeclaration {
	ACCEPT
};

struct Function : ExternalDeclaration {
	Vector<Declaration> declarations;
	CompoundStatement body;
	ACCEPT
};

struct REPLStatement : External {
	Ptr<Statement> statement;
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
