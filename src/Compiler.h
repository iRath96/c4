//
//  Compiler.h
//  c4
//
//  Created by Alexander Rath on 03.12.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#ifndef Compiler_h
#define Compiler_h

#include "AST.h"
#include <functional>
#include <set>
#include <vector>
#include <map>
#include <stack>

using namespace ast;
//using namespace std;

class Scope {
public:
    virtual ~Scope() {}
    
    void error(std::string message) {
        std::cerr << message << std::endl;
    }
};

class BlockScope : public Scope {
public:
    std::map<const char *, TypeName> variables; // @todo ast::Id instead of const char *
    
    void declare(const char *name, TypeName type) {
        variables.insert(std::make_pair(name, type));
    }
    
    bool resolve(const char *name, TypeName &result) {
        auto it = variables.find(name);
        if (it == variables.end())
            return false;
        result = it->second;
        return true;
    }
};

class FileScope : public BlockScope {
public:
    std::map<const char *, ast::Ptr<TypeSpecifier>> composedTypes;
    
protected:
    void declareComposedType(ComposedType *type, ast::Ptr<TypeSpecifier> &ptr) {
        if (!type->isNamed())
            return;
        
        if (composedTypes.find(type->name) != composedTypes.end())
            error("composed type already defined");
        
        composedTypes.insert(std::make_pair(type->name, ptr));
    }
    
    void resolveComposedType(ComposedType *type, ast::Ptr<TypeSpecifier> &ptr) {
        if (type->isQualified()) {
            declareComposedType(type, ptr);
            return;
        }
        
        auto it = composedTypes.find(type->name);
        if (it == composedTypes.end())
            error(std::string(type->name) + " has not been declared before");
        else
            ptr = it->second;
    }
    
public:
    void resolveType(ast::Ptr<TypeSpecifier> &type) {
        if (auto ct = dynamic_cast<ComposedType *>(type.get()))
            resolveComposedType(ct, type);
    }
};

class FunctionScope : public BlockScope {
public:
    std::set<const char *> resolvedLabels;
    std::set<const char *> unresolvedLabels;
    
    void resolveLabel(const char *id) {
        resolvedLabels.insert(id);
        unresolvedLabels.erase(id);
    }
    
    void referenceLabel(const char *id) {
        if (resolvedLabels.find(id) == resolvedLabels.end())
            unresolvedLabels.insert(id);
    }
};

class SwitchScope : public Scope {
public:
};

class IterationScope : public Scope {
public:
};

class ScopeStack {
public:
    std::vector<ast::Ptr<Scope>> stack;
    
    ast::Ptr<Scope> &top() {
        return stack.back();
    }
    
    template<typename T>
    ast::Ptr<T> &push() {
        stack.push_back(std::make_shared<T>());
        return reinterpret_cast<ast::Ptr<T> &>(top());
    }
    
    template<typename T>
    void execute(std::function<void ()> callback) {
        push<T>();
        callback();
        pop();
    }
    
    void pop() {
        stack.pop_back();
    }
    
    template<typename T>
    ast::Ptr<T> find() {
        for (auto it = stack.rbegin(); it != stack.rend(); ++it)
            if (dynamic_cast<T *>(it->get()))
                return reinterpret_cast<ast::Ptr<T> &>(*it);
        
        return nullptr;
    }
    
    bool resolve(const char *name, TypeName &result) {
        for (auto it = stack.rbegin(); it != stack.rend(); ++it)
            if (auto bs = dynamic_cast<BlockScope *>(it->get()))
                if (bs->resolve(name, result))
                    return true;
        return false;
    }
};

class Type {
public:
    TypeName typeName;
    bool lvalue;
    
    bool canCast(TypeName &target) {
        return true; // everything is possible in the wonderful lands of C!
    }
    
    Type cast(TypeName &target) {
        Type t;
        t.typeName = target;
        t.lvalue = lvalue;
        return t;
    }
    
    bool canDereference() { // @todo make declarators more like expressions
        auto &m = typeName.declarator.modifiers;
        return !m.empty() && dynamic_cast<DeclaratorPointer *>(m.back().get());
    }
    
    Type dereference() {
        Type t;
        t.lvalue = true;
        t.typeName = typeName;
        t.typeName.declarator.modifiers.pop_back();
        return t;
    }
    
    bool isScalar() {
        bool result = true;
        for (auto &ts : typeName.specifiers)
            if (auto nt = dynamic_cast<NamedType *>(ts.get()))
                result = true; // @todo
            else
                result = false; // must be composed
        
        for (auto &mod : typeName.declarator.modifiers) {
            if (auto ptr = dynamic_cast<DeclaratorPointer *>(mod.get()))
                result = true;
            else
                result = false;
        }
        
        return result;
    }
    
    bool isComposed() {
        if (!typeName.declarator.modifiers.empty())
            return false;
        return getComposedType();
    }
    
    bool getMember(const char *name, Type &type) {
        auto ct = getComposedType();
        for (auto &declaration : ct->declarations) {
            for (auto &decl : declaration.declarators)
                if (!strcmp(decl.name, name)) {
                    type.lvalue = lvalue;
                    type.typeName.specifiers = declaration.specifiers;
                    type.typeName.declarator = decl;
                    return true;
                }
        }
        
        return false;
    }
    
    ComposedType *getComposedType() {
        for (auto &ts : typeName.specifiers)
            if (auto ct = dynamic_cast<ComposedType *>(ts.get()))
                return ct;
        return NULL;
    }
    
    static bool validateTypeName(TypeName &typeName) {
        bool hasComposed = false, hasScalar = false;
        for (auto &ts : typeName.specifiers)
            if (auto nt = dynamic_cast<NamedType *>(ts.get()))
                hasScalar = true;
            else
                hasComposed = true;
        
        return hasComposed ^ hasScalar;
    }
    
    static Type named(const char *name, bool ptr = false) {
        // @idea use parser here for flexibility
        
        auto n = std::make_shared<NamedType>();
        n->id = name;
        
        TypeName typeName;
        typeName.specifiers.push_back(n);
        
        if (ptr) {
            auto p = std::make_shared<DeclaratorPointer>();
            typeName.declarator.modifiers.push_back(p);
        }
        
        Type type;
        type.typeName = typeName;
        type.lvalue = false;
        return type;
    }
};

class ExpressionStack {
public:
    std::stack<Type> stack;
    
    Type pop() {
        Type t = stack.top();
        stack.pop();
        return t;
    }
    
    Type &push(Type t) {
        stack.push(t);
        return top();
    }
    
    Type &push(TypeName typeName, bool lvalue = false) {
        Type t;
        t.typeName = typeName;
        t.lvalue = lvalue;
        return push(t);
    }
    
    Type &top() {
        return stack.top();
    }
};

class CompilerError {
public:
    std::string message;
    TextPosition pos;
    
    CompilerError(const std::string &message, TextPosition pos)
    : message(message), pos(pos) {
        
    }
};

class Compiler : public Visitor {
protected:
    ScopeStack scopes;
    ExpressionStack exprStack;
    
    void inspect(Node &node) {
        node.accept(*this);
    }
    
    template<typename T>
    void inspect(ast::Ptr<T> &node) {
        if (node.get())
            node->accept(*this);
    }
    
    [[noreturn]] void error(std::string message, Node &node) {
        throw CompilerError(message, node.pos);
    }
    
    Type exprType(Expression &expr) { // @todo assert stack size
        inspect(expr);
        return exprStack.pop();
    }
    
public:
    Compiler() {
        scopes.push<FileScope>();
    }
    
    virtual void visit(CaseLabel &node) {
        if (!scopes.find<SwitchScope>().get())
            error("case outside switch", node);
    }
    
    virtual void visit(DefaultLabel &node) {
        if (!scopes.find<SwitchScope>().get())
            error("default outside switch", node);
    }
    
    virtual void visit(IdentifierLabel &node) {
        auto scope = scopes.find<FunctionScope>();
        if (scope.get())
            scope->resolveLabel(node.id);
        else
            error("label outside of function?!", node);
    }
    
    virtual void visit(GotoStatement &node) {
        auto scope = scopes.find<FunctionScope>();
        if (scope.get())
            scope->referenceLabel(node.target);
        else
            error("goto outside of function?!", node);
    }
    
    virtual void visit(ContinueStatement &node) {
        if (!scopes.find<IterationScope>().get())
            error("break/continue outside of iteration stmt", node);
    }
    
    virtual void visit(Identifier &) {}

    virtual void visit(CompoundStatement &node) {
        scopes.execute<BlockScope>([&]() {
            for (auto &item : node.items)
                inspect(item);
        });
    }
    
    virtual void visit(DeclaratorPointer &) {}
    virtual void visit(DeclaratorParameterList &) {}
    virtual void visit(DeclaratorIdentifierList &) {}
    virtual void visit(Declarator &) {}
    
    void resolveTypeSpecifiers(ast::PtrVector<ast::TypeSpecifier> &specifiers) {
        auto scope = scopes.find<FileScope>();
        for (auto &spec : specifiers)
            scope->resolveType(spec);
    }
    
    virtual void visit(TypeName &node) {
        resolveTypeSpecifiers(node.specifiers);
    }

    virtual void visit(Declaration &node) {
        auto scope = scopes.find<BlockScope>();
        resolveTypeSpecifiers(node.specifiers);
        
        for (auto &decl : node.declarators) {
            inspect(decl);
            
            TypeName type;
            type.specifiers = node.specifiers;
            type.declarator = decl;
            
            if (!Type::validateTypeName(type))
                error("invalid type", node);
            
            // find identifier
            if (decl.isAbstract())
                error("abstract declarator in declaration", node);
            else
                scope->declare(decl.name, type);
        }
    }

    virtual void visit(ExternalDeclarationVariable &node) {
        visit((Declaration &)node);
    }

    virtual void visit(ExternalDeclarationFunction &node) {
        visit((Declaration &)node);
        scopes.execute<FunctionScope>([&]() {
            inspect(node.body);
        });
    }
    
    virtual void visit(ParameterDeclaration &) {}

#pragma mark - Expressions

    virtual void visit(ConstantExpression &node) {
        if (node.isIdentifier) {
            TypeName tn;
            if (!scopes.resolve(node.text, tn))
                error("identifier could not be resolved", node);
            
            exprStack.push(tn, true);
            return;
        }
        
        switch (node.text[0]) {
        case '\'': exprStack.push(Type::named("char")); break;
        case '\"': exprStack.push(Type::named("char", true)); break;
        default: exprStack.push(Type::named("int")); break;
        }
    }
    
    virtual void visit(CastExpression &node) {
        auto type = exprType(*node.expression);
        
        if (!type.canCast(node.type))
            error("invalid cast", node);
        
        exprStack.push(type.cast(node.type));
    }

    virtual void visit(UnaryExpression &node) {
        auto type = exprType(*node.operand);
        
        switch (node.op) {
        case Token::Punctuator::ASTERISK:
            if (!type.canDereference())
                error("cannot dereference non-pointer", node);
            exprStack.push(type.dereference());
            break;
            
        default:
            error("unary operation is not supported", node);
            exprStack.push(type);
            break;
        }
    }

    virtual void visit(BinaryExpression &node) {
        auto lhs = exprType(*node.lhs);
        auto rhs = exprType(*node.rhs);
        
        if (!lhs.isScalar()) error("lhs is not scalar", *node.lhs);
        if (!rhs.isScalar()) error("rhs is not scalar", *node.rhs);
        
        if (Token::precedence(node.op) == Token::Precedence::ASSIGNMENT)
            if (!lhs.lvalue)
                error("lhs is not an lvalue", *node.lhs);
        
        exprStack.push(lhs);
    }

    virtual void visit(ConditionalExpression &node) {
        auto cond = exprType(*node.condition);
        if (!cond.isScalar())
            error("condition not scalar", *node.condition);
    }

    virtual void visit(ExpressionList &node) {
        Type lastType;
        for (auto &child : node.children)
            lastType = exprType(*child);
        exprStack.push(lastType);
    }

    virtual void visit(CallExpression &node) {
        error("call expressions not yet supported", node);
    }
    
    virtual void visit(SubscriptExpression &node) {
        error("subscript expressions not yet supported", node);
    }
    
    virtual void visit(MemberExpression &node) {
        Type base = exprType(*node.base);
        if (node.dereference) {
            if (!base.canDereference()) error("cannot dereference base", node);
            base = base.dereference();
        }
        
        Type memberType;
        
        if (!base.isComposed()) error("member access into non-composed type", node);
        if (!base.getMember(node.id, memberType)) error("unknown member " + std::string(node.id), node);
        
        exprStack.push(memberType);
    }
    
    virtual void visit(PostExpression &node) {
        error("post expressions not yet supported", node);
    }

    virtual void visit(ExpressionStatement &node) {
        exprType(node.expressions);
    }

    virtual void visit(SizeofExpressionTypeName &node) {
        exprStack.push(Type::named("int"));
    }

    virtual void visit(SizeofExpressionUnary &node) {
        exprType(*node.expression);
        exprStack.push(Type::named("int"));
    }

    virtual void visit(ComposedType &) {}
    virtual void visit(NamedType &) {}

    virtual void visit(DesignatorWithIdentifier &) {}
    virtual void visit(DesignatorWithExpression &) {}
    virtual void visit(Initializer &) {}
    virtual void visit(InitializerList &) {}
    virtual void visit(InitializerExpression &) {}

    virtual void visit(IterationStatement &node) {
        scopes.execute<IterationScope>([&]() {
            inspect(node.body);
        });
    }
    
    virtual void visit(SelectionStatement &node) {
        auto cond = exprType(node.condition);
        if (!cond.isScalar())
            error("condition not scalar", node.condition);
        
        inspect(node.when_true);
        inspect(node.when_false);
    }

    virtual void visit(ReturnStatement &node) {
        auto type = exprType(node.expressions);
        error("unable to verify return type", node);
    }
};

#endif /* Compiler_h */
