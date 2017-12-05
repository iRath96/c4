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

#include <iostream>
#include <functional>
#include <set>
#include <vector>
#include <map>
#include <stack>

using namespace ast;

class CompilerError {
public:
    std::string message;
    lexer::TextPosition pos;
    
    CompilerError(const std::string &message, lexer::TextPosition pos)
    : message(message), pos(pos) {
        
    }
};

class Scope {
public:
    virtual ~Scope() {}
};

class Type;
class BlockScope : public Scope {
public:
    std::map<std::string, Ptr<Type>> variables;
    
    void declare(std::string name, Ptr<Type> type, lexer::TextPosition pos) {
        for (auto &v : variables)
            if (v.first == name)
                throw CompilerError("variable " + name + " redefined", pos);
        
        variables.insert(std::make_pair(name, type));
    }
    
    bool resolve(std::string name, Ptr<Type> &result) {
        auto it = variables.find(name);
        if (it == variables.end())
            return false;
        result = it->second;
        return true;
    }
};

class FileScope : public BlockScope {
public:
    std::map<std::string, ast::Ptr<TypeSpecifier>> composedTypes;
    
protected:
    void declareComposedType(ast::ComposedType *type, ast::Ptr<TypeSpecifier> &ptr) {
        if (!type->isNamed())
            return;
        
        if (composedTypes.find(type->name) != composedTypes.end())
            throw CompilerError("type " + std::string(type->name) + " redefined", type->pos);
        
        composedTypes.insert(std::make_pair(type->name, ptr));
    }
    
    void resolveComposedType(ast::ComposedType *type, ast::Ptr<TypeSpecifier> &ptr) {
        if (type->isQualified()) {
            declareComposedType(type, ptr);
            return;
        }
        
        auto it = composedTypes.find(type->name);
        if (it == composedTypes.end())
            throw CompilerError(std::string(type->name) + " has not been declared before", type->pos);
        else
            ptr = it->second;
    }
    
public:
    void resolveType(ast::Ptr<TypeSpecifier> &type) {
        if (auto ct = dynamic_cast<ast::ComposedType *>(type.get()))
            resolveComposedType(ct, type);
    }
};

class FunctionScope : public BlockScope {
public:
    Ptr<Type> returnType;
    
    std::set<std::string> resolvedLabels;
    std::map<std::string, lexer::TextPosition> unresolvedLabels;
    
    void resolveLabel(std::string id, lexer::TextPosition pos) {
        if (resolvedLabels.find(id) != resolvedLabels.end())
            throw CompilerError("label " + id + " redefined", pos);
        
        resolvedLabels.insert(id);
        unresolvedLabels.erase(id);
    }
    
    void referenceLabel(std::string id, lexer::TextPosition pos) {
        if (resolvedLabels.find(id) == resolvedLabels.end())
            unresolvedLabels.insert(std::make_pair(id, pos));
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
    
    bool resolve(std::string name, Ptr<Type> &result) {
        for (auto it = stack.rbegin(); it != stack.rend(); ++it)
            if (auto bs = dynamic_cast<BlockScope *>(it->get()))
                if (bs->resolve(name, result))
                    return true;
        return false;
    }
};

class Type : public std::enable_shared_from_this<Type> {
public:
    static Ptr<Type> ptrdiffType;
    
    Ptr<Type> cast(const TypeName &target, lexer::TextPosition pos) const {
        Ptr<Type> type = Type::create(target.specifiers, target.declarator, target.pos);
        if (!isCompatible(*type))
            throw CompilerError("casting incompatible types", pos);
        return type;
    }
    
    virtual Ptr<Type> dereference(lexer::TextPosition pos) {
        throw CompilerError("cannot dereference non-pointer", pos);
    }
    
    virtual Ptr<Type> call(ast::PtrVector<Type> argTypes, lexer::TextPosition pos) const {
        throw CompilerError("cannot call non-function", pos);
    }
    
    virtual Ptr<Type> getMember(std::string name, lexer::TextPosition pos) const {
        throw CompilerError("cannot access member in non-composed type", pos);
    }
    
    Ptr<Type> reference();
    
    static Ptr<Type> create(const PtrVector<TypeSpecifier> &specifiers, lexer::TextPosition pos);
    static Ptr<Type> create(const PtrVector<TypeSpecifier> &specifiers, const Declarator &decl, lexer::TextPosition pos);
    
    Ptr<Type> applyDeclarator(Declarator decl);
    
    virtual bool isScalar() = 0;
    virtual bool isCompatible(const Type &other) const = 0;
    
    static Ptr<Type> add(Ptr<Type> &a, Ptr<Type> &b, lexer::TextPosition pos);
    static Ptr<Type> subtract(Ptr<Type> &a, Ptr<Type> &b, lexer::TextPosition pos);
    
    static bool canCompare(const Type &a, const Type &b);
};

class TypePair {
public:
    bool lvalue;
    Ptr<Type> type;
    
    TypePair() {}
    TypePair(bool lvalue, Ptr<Type> type) : lvalue(lvalue), type(type) {}
};

class ArithmeticType : public Type {
public:
    enum Sign {
        SIGNED = 1,
        UNSIGNED = 0
    };
    
    enum Size {
        LONG  = 8,
        INT   = 4,
        SHORT = 2,
        CHAR  = 1
    };
    
    static Sign max(Sign a, Sign b) {
        return (Sign)((int)a > (int)b ? a : b);
    }
    
    static Size max(Size a, Size b) {
        return (Size)((int)a > (int)b ? a : b);
    }
    
    Sign sign;
    Size size;
    
    ArithmeticType() : sign(UNSIGNED), size(INT) {}
    ArithmeticType(Sign sign, Size size) : sign(sign), size(size) {}
    
    virtual bool isScalar() { return true; }
    virtual bool isCompatible(const Type &other) const {
        return dynamic_cast<const ArithmeticType *>(&other);
    }
};

class NullPointerType : public ArithmeticType {
public:
    virtual bool isCompatible(const Type &other) const;
};

class ComposedType : public Type {
public:
    lexer::Token::Keyword type; // @todo
    std::vector<std::pair<std::string, Ptr<Type>>> members;
    
    virtual bool isScalar() { return false; }
    virtual bool isCompatible(const Type &other) const {
        auto c = dynamic_cast<const ComposedType *>(&other);
        if (!c || c->members.size() != members.size())
            return false;
        
        for (int i = 0; i < members.size(); ++i)
            if (!members[i].second->isCompatible(*c->members[i].second))
                return false;
        
        return true;
    }
    
    virtual Ptr<Type> getMember(std::string name, lexer::TextPosition pos) const {
        for (auto &member : members) { // @todo not efficient
            if (member.first == name)
                return member.second;
        }
        
        throw CompilerError("unknown member " + name, pos);
    }
    
    void addMember(std::string name, Ptr<Type> type, lexer::TextPosition pos) {
        for (auto &member : members)
            if (member.first == name)
                throw CompilerError("member " + name + " redefined", pos);
        
        members.push_back(std::make_pair(std::string(name), type));
    }
};

class FunctionType : public Type {
public:
    Ptr<Type> returnType;
    std::vector<Ptr<Type>> parameters;
    
    virtual bool isScalar() { return false; }
    virtual bool isCompatible(const Type &other) const;
    
    virtual Ptr<Type> call(ast::PtrVector<Type> argTypes, lexer::TextPosition pos) const {
        if (argTypes.size() != parameters.size())
            throw CompilerError(
                std::to_string(argTypes.size()) + " arguments given, " +
                std::to_string(parameters.size()) + " expected"
            , pos);
        return returnType;
    }
};

class PointerType : public Type {
public:
    Ptr<Type> base;
    Vector<const char *> qualifiers;
    
    PointerType() {}
    PointerType(Ptr<Type> base) : base(base) {}
    
    virtual bool isScalar() { return true; }
    virtual bool isCompatible(const Type &other) const;
    
    virtual Ptr<Type> dereference(lexer::TextPosition pos) {
        return base;
    }
    
    virtual Ptr<Type> call(ast::PtrVector<Type> argTypes, lexer::TextPosition pos) const {
        return base->call(argTypes, pos);
    }
};

class VoidType : public Type {
public:
    virtual bool isScalar() { return false; }
    virtual bool isCompatible(const Type &other) const {
        return dynamic_cast<const VoidType *>(&other);
    }
};

class ExpressionStack {
public:
    std::stack<TypePair> stack;
    
    TypePair pop() {
        TypePair t = stack.top();
        stack.pop();
        return t;
    }
    
    TypePair &push(TypePair t) {
        stack.push(t);
        return top();
    }
    
    TypePair &top() {
        return stack.top();
    }
};

class Compiler : public Visitor {
protected:
    ScopeStack scopes;
    ExpressionStack exprStack;
    
    void inspect(Node &node) {
        //std::cout << node.pos.line << ":" << node.pos.column << std::endl;
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
    
    TypePair exprType(Expression &expr) { // @todo assert stack size
        inspect(expr);
        return exprStack.pop();
    }
    
    TypePair intType, charType, stringType, voidType, nullptrType;
    
public:
    Compiler() {
        scopes.push<FileScope>();
        
        intType = TypePair(false, std::make_shared<ArithmeticType>(ArithmeticType::UNSIGNED, ArithmeticType::INT));
        charType = TypePair(false, std::make_shared<ArithmeticType>(ArithmeticType::SIGNED, ArithmeticType::CHAR));
        stringType = TypePair(false, std::make_shared<PointerType>(charType.type));
        voidType = TypePair(false, std::make_shared<VoidType>());
        nullptrType = TypePair(false, std::make_shared<NullPointerType>());
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
            scope->resolveLabel(node.id, node.pos);
        else
            error("label outside of function?!", node);
    }
    
    void visit(Statement &node) {
        for (auto &lab : node.labels)
            inspect(lab);
    }
    
    virtual void visit(GotoStatement &node) {
        visit((Statement &)node);
        
        auto scope = scopes.find<FunctionScope>();
        if (scope.get())
            scope->referenceLabel(node.target, node.pos);
        else
            error("goto outside of function?!", node);
    }
    
    virtual void visit(ContinueStatement &node) {
        visit((Statement &)node);
        
        if (!scopes.find<IterationScope>().get())
            error("break/continue outside of iteration stmt", node);
    }
    
    virtual void visit(Identifier &) {}

    virtual void visitBlockItems(CompoundStatement &node) {
        for (auto &item : node.items)
            inspect(item);
    }
    
    virtual void visit(CompoundStatement &node) {
        visit((Statement &)node);
        
        scopes.execute<BlockScope>([&]() {
            visitBlockItems(node);
        });
    }
    
    virtual void visit(DeclaratorPointer &) {}
    virtual void visit(DeclaratorParameterList &) {}
    virtual void visit(Declarator &) {}
    
    bool resolveTypeSpecifiers(ast::PtrVector<ast::TypeSpecifier> &specifiers) {
         // @todo be more efficient and reuse ::ComposedTypes
        auto scope = scopes.find<FileScope>();
        bool result = false;
        for (auto &spec : specifiers) {
            if (auto ct = dynamic_cast<const ast::ComposedType *>(spec.get()))
                if (ct->isNamed() && ct->isQualified())
                    result = true;
            
            scope->resolveType(spec);
        }
        
        return result;
    }
    
    virtual void visit(TypeName &node) {
        resolveTypeSpecifiers(node.specifiers);
    }

    virtual void visit(Declaration &node) {
        auto scope = scopes.find<BlockScope>();
        auto specifiers = node.specifiers;
        bool declaredStruct = resolveTypeSpecifiers(specifiers);
        
        if (node.declarators.empty()) {
            if (!declaredStruct)
                error("declaration does not declare anything", node);
            return;
        }
        
        Ptr<Type> type = Type::create(specifiers, node.pos);
        for (auto &decl : node.declarators) {
            inspect(decl);
            Ptr<Type> dtype = type->applyDeclarator(decl);
            
            // find identifier
            if (decl.isAbstract())
                // @todo assert(false) here
                error("abstract declarator in declaration", node);
            else
                scope->declare(decl.name, dtype, decl.pos);
            
            if (decl.initializer.get()) {
                auto itp = exprType(*decl.initializer);
                if (!itp.type->isCompatible(*dtype))
                    error("invalid initialization", *decl.initializer);
            }
        }
    }

    virtual void visit(ExternalDeclarationVariable &node) {
        visit((Declaration &)node);
    }

    virtual void visit(ExternalDeclarationFunction &node) {
        visit((Declaration &)node);
        
        auto &decl = node.declarators.front();
        if (decl.modifiers.empty())
            error("no parameter list given", node);
        
        scopes.execute<FunctionScope>([&]() {
            auto scope = scopes.find<FunctionScope>(); // @todo as param?
            
            auto t = Type::create(node.specifiers, decl, node.pos);
            auto fn = dynamic_cast<FunctionType *>(t.get());
            scope->returnType = fn->returnType;
            
            if (auto plist = dynamic_cast<DeclaratorParameterList *>(decl.modifiers.back().get())) {
                for (auto &param : plist->parameters) {
                    if (param.declarator.isAbstract())
                        error("parameter without name", param);
                    
                    inspect(param);
                }
            } else
                error("no parameter list given", node);
            
            for (auto &declaration : node.declarations)
                inspect(declaration);
            
            visitBlockItems(node.body);
            
            if (!scope->unresolvedLabels.empty()) {
                auto lab = *scope->unresolvedLabels.begin();
                throw CompilerError("could not resolve label " + lab.first, lab.second);
            }
        });
    }
    
    virtual void visit(ParameterDeclaration &node) {
        if (node.declarator.isAbstract())
            return;
        
        auto scope = scopes.find<FunctionScope>();
        scope->declare(node.declarator.name, Type::create(node.specifiers, node.declarator, node.pos), node.declarator.pos);
    }

#pragma mark - Expressions

    virtual void visit(ConstantExpression &node) {
        if (node.isIdentifier) {
            Ptr<Type> type;
            if (!scopes.resolve(node.text, type))
                error(std::string(node.text) + " was not declared in this scope", node);
            
            exprStack.push(TypePair(true, type));
            return;
        }
        
        switch (node.text[0]) {
        case '\'': exprStack.push(charType); break;
        case '\"': exprStack.push(stringType); break;
        default: exprStack.push(!strcmp(node.text, "0") ? nullptrType : intType); break;
        }
    }
    
    virtual void visit(CastExpression &node) {
        auto tp = exprType(*node.expression);
        exprStack.push(TypePair(tp.lvalue, tp.type->cast(node.type, node.pos)));
    }

    virtual void visit(UnaryExpression &node) {
        using Punctuator = lexer::Token::Punctuator;
        
        auto tp = exprType(*node.operand);
        
        switch (node.op) {
        case Punctuator::ASTERISK:
            exprStack.push(TypePair(true, tp.type->dereference(node.pos)));
            break;
        
        case Punctuator::BIT_AND:
            if (!tp.lvalue)
                error("cannot reference non lvalue", node);
            exprStack.push(TypePair(false, tp.type->reference()));
            break;
        
        default:
            //error("unary operation is not supported", node);
            exprStack.push(tp);
            break;
        }
    }

    virtual void visit(BinaryExpression &node) {
        using namespace lexer;
        using Op = lexer::Token::Punctuator;
        using Prec = lexer::Token::Precedence;
        
        auto lhs = exprType(*node.lhs);
        auto rhs = exprType(*node.rhs);
        
        if (Token::precedence(node.op) == Token::Precedence::ASSIGNMENT)
            if (!lhs.lvalue)
                error("lhs is not an lvalue", *node.lhs);
        
        switch (node.op) {
        case Op::PLUS: exprStack.push(TypePair(false, Type::add(lhs.type, rhs.type, node.pos))); break;
        case Op::MINUS: exprStack.push(TypePair(false, Type::subtract(lhs.type, rhs.type, node.pos))); break;
        default:
            switch (Token::precedence(node.op)) {
            case Prec::MULTIPLICATIVE: // integer for %, arithmetic otherwise
            case Prec::SHIFT: // integer
            case Prec::AND:
            case Prec::INCLUSIVE_OR:
            case Prec::EXCLUSIVE_OR:
            {
                auto a = dynamic_cast<ArithmeticType *>(lhs.type.get());
                auto b = dynamic_cast<ArithmeticType *>(lhs.type.get());
                if (!a || !b) error("operands must be arithmetic", node);
                exprStack.push(TypePair(a, lhs.type)); // @todo?
                break;
            }
            
            case Prec::LOGICAL_OR:
            case Prec::LOGICAL_AND:
                if (!lhs.type->isScalar() || !rhs.type->isScalar()) error("operands must be scalar", node);
                exprStack.push(intType);
                break;
            
            case Prec::ASSIGNMENT:
                if (!lhs.lvalue) error("lhs is not an lvalue", *node.lhs);
                if (!lhs.type->isCompatible(*rhs.type)) error("assignment with incompatible type", *node.rhs);
                exprStack.push(TypePair(false, lhs.type));
                break;
            
            case Prec::EQUALITY: {
                if (!Type::canCompare(*lhs.type, *rhs.type)) error("invalid comparison", node);
                exprStack.push(intType);
                break;
            }
            
            case Prec::RELATIONAL:
                if (!lhs.type->isCompatible(*rhs.type)) error("comparison of incompatible types", node);
                exprStack.push(intType);
                break;
            
            default:
                error("operator not implemented", node);
            }
        }
    }

    virtual void visit(ConditionalExpression &node) {
        auto cond = exprType(*node.condition);
        if (!cond.type->isScalar())
            error("condition not scalar", *node.condition);
        
        auto lhs = exprType(*node.when_true);
        auto rhs = exprType(*node.when_false);
        
        // @todo compare lhs and rhs type
        
        exprStack.push(lhs);
    }

    virtual void visit(ExpressionList &node) {
        TypePair lastType = voidType;
        for (auto &child : node.children)
            lastType = exprType(*child);
        exprStack.push(lastType);
    }

    virtual void visit(CallExpression &node) {
        auto tp = exprType(*node.function);
        
        PtrVector<Type> argumentTypes;
        for (auto &arg : node.arguments) {
            auto tp = exprType(*arg);
            argumentTypes.push_back(tp.type);
        }
        
        exprStack.push(TypePair(false, tp.type->call(argumentTypes, node.pos)));
    }
    
    virtual void visit(SubscriptExpression &node) {
        auto base = exprType(*node.base);
        // @todo this is actually incorrect, need to add first
        
        auto subscript = exprType(node.subscript);
        if (!subscript.type->isScalar())
            error("invalid subscript", node);
        
        exprStack.push(TypePair(true, base.type->dereference(node.pos)));
    }
    
    virtual void visit(MemberExpression &node) {
        auto base = exprType(*node.base);
        
        if (node.dereference) {
            base.lvalue = true;
            base.type = base.type->dereference(node.pos);
        }
        
        exprStack.push(TypePair(base.lvalue, base.type->getMember(node.id, node.pos)));
    }
    
    virtual void visit(PostExpression &node) {
        auto t = exprType(*node.base);
        // @todo test if this makes sense
        t.lvalue = false;
        exprStack.push(t);
    }

    virtual void visit(ExpressionStatement &node) {
        visit((Statement &)node);
        exprType(node.expressions);
    }

    virtual void visit(SizeofExpressionTypeName &node) {
        exprStack.push(intType);
    }

    virtual void visit(SizeofExpressionUnary &node) {
        exprType(*node.expression);
        exprStack.push(intType);
    }

    virtual void visit(ast::ComposedType &) {}
    virtual void visit(NamedType &) {}

    virtual void visit(DesignatorWithIdentifier &) {}
    virtual void visit(DesignatorWithExpression &) {}
    
    virtual void visit(Initializer &node) { error("not supported", node); }
    virtual void visit(InitializerList &node) { error("not supported", node); }
    
    virtual void visit(InitializerExpression &node) {
        exprStack.push(TypePair(false, Type::create(node.type.specifiers, node.type.declarator, node.pos)));
    }

    virtual void visit(IterationStatement &node) {
        visit((Statement &)node);
        scopes.execute<IterationScope>([&]() {
            inspect(node.body);
        });
    }
    
    virtual void visit(SelectionStatement &node) {
        visit((Statement &)node);
        
        auto cond = exprType(node.condition);
        if (!cond.type->isScalar())
            error("condition not scalar", node.condition);
        
        inspect(node.when_true);
        inspect(node.when_false);
    }

    virtual void visit(ReturnStatement &node) {
        visit((Statement &)node);
        
        auto scope = scopes.find<FunctionScope>();
        auto expectedType = scope->returnType;
        auto givenType = exprType(node.expressions);
        
        if (!expectedType->isCompatible(*givenType.type))
            throw CompilerError("illegal return", node.pos);
    }
};

#endif /* Compiler_h */
