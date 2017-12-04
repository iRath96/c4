//
//  AST.h
//  c4
//
//  Created by Alexander Rath on 12.11.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

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

// Nodes
struct Identifier;
struct TypeName;

// Labels
struct CaseLabel;
struct DefaultLabel;
struct IdentifierLabel;

// Declarators
struct DeclaratorParameterList;
struct DeclaratorPointer;
struct Declarator;

// Designators
struct DesignatorWithIdentifier;
struct DesignatorWithExpression;
struct Initializer;

// Expressions
struct ConstantExpression;
struct CastExpression;
struct UnaryExpression;
struct BinaryExpression;
struct ConditionalExpression;
struct ExpressionList;
struct CallExpression;
struct SubscriptExpression;
struct MemberExpression;
struct PostExpression;
struct InitializerList;
struct InitializerExpression;
struct SizeofExpressionUnary;
struct SizeofExpressionTypeName;

// Statements
struct CompoundStatement;
struct IterationStatement;
struct ExpressionStatement;
struct SelectionStatement;
struct GotoStatement;
struct ContinueStatement;
struct ReturnStatement;

// Declarations
struct Declaration;
struct ParameterDeclaration;
struct ExternalDeclarationVariable;
struct ExternalDeclarationFunction;

// Types
struct NamedType;
struct ComposedType;

// Visitor
struct Visitor {
    // Nodes
    virtual void visit(Identifier &) = 0;
    virtual void visit(TypeName &) = 0;

    // Labels
    virtual void visit(CaseLabel &) = 0;
    virtual void visit(DefaultLabel &) = 0;
    virtual void visit(IdentifierLabel &) = 0;

    // Declarators
    virtual void visit(DeclaratorParameterList &) = 0;
    virtual void visit(DeclaratorPointer &) = 0;
    virtual void visit(Declarator &) = 0;

    // Designators
    virtual void visit(DesignatorWithIdentifier &) = 0;
    virtual void visit(DesignatorWithExpression &) = 0;
    virtual void visit(Initializer &) = 0;

    // Expressions
    virtual void visit(ConstantExpression &) = 0;
    virtual void visit(CastExpression &) = 0;
    virtual void visit(UnaryExpression &) = 0;
    virtual void visit(BinaryExpression &) = 0;
    virtual void visit(ConditionalExpression &) = 0;
    virtual void visit(ExpressionList &) = 0;
    virtual void visit(CallExpression &) = 0;
    virtual void visit(SubscriptExpression &) = 0;
    virtual void visit(MemberExpression &) = 0;
    virtual void visit(PostExpression &) = 0;
    virtual void visit(InitializerList &) = 0;
    virtual void visit(InitializerExpression &) = 0;
    virtual void visit(SizeofExpressionUnary &) = 0;
    virtual void visit(SizeofExpressionTypeName &) = 0;

    // Statements
    virtual void visit(CompoundStatement &) = 0;
    virtual void visit(IterationStatement &) = 0;
    virtual void visit(ExpressionStatement &) = 0;
    virtual void visit(SelectionStatement &) = 0;
    virtual void visit(GotoStatement &) = 0;
    virtual void visit(ContinueStatement &) = 0;
    virtual void visit(ReturnStatement &) = 0;

    // Declarations
    virtual void visit(Declaration &) = 0;
    virtual void visit(ParameterDeclaration &) = 0;
    virtual void visit(ExternalDeclarationVariable &) = 0;
    virtual void visit(ExternalDeclarationFunction &) = 0;

    // Types
    virtual void visit(NamedType &) = 0;
    virtual void visit(ComposedType &) = 0;
};

#pragma mark - Base classes

#define ACCEPT virtual void accept(Visitor &v) { v.visit(*this); }

struct Node {
    lexer::TextPosition pos;
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
    const char *id;
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
    const char *id;
    ACCEPT
};

#pragma mark - Declarators

struct DeclaratorModifier : Node {};

struct DeclaratorParameterList : DeclaratorModifier {
    ast::Vector<ast::ParameterDeclaration> parameters;
    ACCEPT
};

struct DeclaratorPointer : DeclaratorModifier {
    Vector<const char *> qualifiers;
    ACCEPT
};

struct Declarator : Node {
    PtrVector<DeclaratorModifier> modifiers;
    Ptr<Expression> initializer;
    
    const char *name = NULL;
    bool isAbstract() const {
        return !name;
    }
    
    ACCEPT
};

struct TypeName : Node {
    PtrVector<TypeSpecifier> specifiers;
    Declarator declarator;
    
    ACCEPT
};

#pragma mark - Designators

struct Designator : Node {};
struct DesignatorWithIdentifier : Designator {
    const char *id;
    ACCEPT
};

struct DesignatorWithExpression : Designator {
    Ptr<Expression> expression;
    ACCEPT
};

struct Initializer : Node {
    PtrVector<Designator> designators;
    Declarator declarator;
    
    ACCEPT
};

#pragma mark - Expressions

struct ConstantExpression : Expression {
    const char *text;
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
    const char *id;
    
    ACCEPT
};

struct PostExpression : Expression {
    Ptr<Expression> base;
    lexer::Token::Punctuator op;
    
    ACCEPT
};

struct InitializerList : Expression { // @todo really inherit from Expression?
    Vector<Initializer> initializers;
    ACCEPT
};

struct InitializerExpression : Expression {
    TypeName type;
    InitializerList initializers;
    
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
    const char *target;
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

struct NamedType : TypeSpecifier {
    const char *id;
    lexer::Token::Keyword keyword;
    
    ACCEPT
};

struct ComposedType : TypeSpecifier {
    const char *name = NULL;
    
    lexer::Token::Keyword type; // STRUCT or UNION
    Vector<Declaration> declarations;
    
    bool isQualified() const {
        return !declarations.empty();
    }
    
    bool isNamed() const {
        return name;
    }
    
    ACCEPT
};

#undef ACCEPT

}

#endif /* AST_h */
