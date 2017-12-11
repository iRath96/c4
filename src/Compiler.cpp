//
//  Compiler.cpp
//  c4
//
//  Created by Alexander Rath on 04.12.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include <stdio.h>
#include "Compiler.h"

void FileScope::close() {
    BlockScope::close();

    for (auto &ut : unresolvedTentative)
        if (!ut.first->isComplete())
            throw CompilerError(
                "tentative definition has type '" + ut.first->describe()  + "' that is never completed"
            , ut.second);
}

Ptr<Type> BlockScope::resolveComposedType(ComposedTypeSpecifier *ct) {
    auto it = composedTypes.find(ct->name);
    if (it == composedTypes.end()) {
        auto type = std::make_shared<ComposedType>();
        type->name = ct->name;
        type->kind = ct->kind;
        type->pos = ct->pos;
        
        Ptr<Type> t = type;
        composedTypes.insert(std::make_pair(std::string(ct->name), t));
        return t;
    } else
        return it->second;
}

Ptr<Type> Type::reference() {
    return std::make_shared<PointerType>(shared_from_this());
}

Ptr<Type> Type::create(const PtrVector<TypeSpecifier> &specifiers, lexer::TextPosition pos, ScopeStack &scopes) {
    using Keyword = lexer::Token::Keyword;

    if (specifiers.empty())
        // invalid type
        throw CompilerError("no type specifiers", pos);

    ArithmeticType::Size size = ArithmeticType::INT;
    ArithmeticType::Sign sign = ArithmeticType::SIGNED;
    bool hasInt = false, hasSize = false, hasSign = false;
    
    ComposedTypeSpecifier *comp = NULL;

    for (auto &spec : specifiers) {
        if (auto nt = dynamic_cast<NamedTypeSpecifier *>(spec.get())) {
            switch (nt->keyword) {
            case Keyword::LONG:
                if (hasSize) {
                    if (size == ArithmeticType::LONG)
                        size = ArithmeticType::LONG_LONG;
                    else
                        throw CompilerError("cannot combine with previous declaration specifier", spec->pos);
                } else {
                    hasSize = true;
                    size = ArithmeticType::LONG;
                }
                
                break;
            
            case Keyword::SHORT:
            case Keyword::CHAR: {
                auto s = nt->keyword == Keyword::SHORT ? ArithmeticType::SHORT : ArithmeticType::CHAR;
                if (hasSize && (s == ArithmeticType::CHAR || s != size))
                    throw CompilerError("cannot combine with previous declaration specifier", spec->pos);
                else {
                    hasSize = true;
                    size = s;
                    break;
                }
            }
                
            case Keyword::INT:
                if (hasInt)
                    throw CompilerError("cannot combine with previous 'int' declaration specifier", spec->pos);
                hasInt = true;
                break;
            
            case Keyword::SIGNED:
            case Keyword::UNSIGNED: {
                auto s = nt->keyword == Keyword::SIGNED ? ArithmeticType::SIGNED : ArithmeticType::UNSIGNED;
                if (hasSign && sign != s)
                    throw CompilerError("cannot combine with previous declaration specifier", spec->pos);
                hasSign = true;
                sign = s;
                break;
            }
                
            case Keyword::VOID:
                if (specifiers.size() != 1)
                    throw CompilerError("void conflict", spec->pos);
                return std::make_shared<VoidType>();
                
            default:
                throw CompilerError("unknown type", spec->pos);
            }
        } else if (auto ct = dynamic_cast<ComposedTypeSpecifier *>(spec.get())) {
            if (comp)
                // multiple composed types
                throw CompilerError("multiple composed types specified", spec->pos);
            comp = ct;
        } else
            // unknown AST type
            throw CompilerError("unimplemented", spec->pos);
    }

    bool isArithmetic = hasInt || hasSign || hasSize;
    bool isComposed = comp;

    if (isComposed && isArithmetic)
        // conflicting type
        throw CompilerError("conflicting type", pos);

    if (isArithmetic) {
        auto s = std::make_shared<ArithmeticType>();
        s->sign = sign;
        s->size = size;
        return s;
    } else {
        Ptr<Type> c;
        if (comp->isNamed())
            c = scopes.resolveComposedType(comp, comp->isQualified());
        else {
            // anonymous
            c = std::make_shared<ComposedType>();
            auto &cc = dynamic_cast<ComposedType &>(*c);
            cc.kind = comp->kind;
            cc.pos = pos;
        }
        
        auto &cc = dynamic_cast<ComposedType &>(*c);
        if (!comp->isQualified())
            return c;
        
        bool isNested = typeQueue.find(&cc) != typeQueue.end();
        if (cc.isComplete() || isNested)
            throw CompilerError(
                std::string(isNested ? "nested " : "") +
                "redefinition of '" + std::string(comp->name) + "'",
                comp->pos
            );
        
        typeQueue.insert(&cc);
        
        for (auto &declaration : comp->declarations) {
            Ptr<Type> type = Type::create(declaration.specifiers, declaration.pos, scopes);
            for (auto &decl : declaration.declarators) {
                Ptr<Type> dtype = type->applyDeclarator(decl, scopes);
                if (!dtype->isComplete())
                    throw CompilerError("field has incomplete type '" + dtype->describe() + "'", decl.pos);
                
                cc.addMember(std::string(decl.name), dtype, decl.pos);
            }
        }
        
        typeQueue.erase(&cc);
        
        cc.markAsComplete();
        
        return c;
    }
}

Ptr<Type> Type::create(const PtrVector<TypeSpecifier> &specifiers, const Declarator &decl, lexer::TextPosition pos, ScopeStack &scopes) {
    return create(specifiers, pos, scopes)->applyDeclarator(decl, scopes);
}

Ptr<Type> Type::applyDeclarator(Declarator decl, ScopeStack &scopes) {
    Ptr<Type> result = shared_from_this();
    
    for (auto &mod : decl.modifiers) {
        if (auto pt = dynamic_cast<DeclaratorPointer *>(mod.get())) {
            auto p = std::make_shared<PointerType>();
            p->qualifiers = pt->qualifiers;
            p->base = result;
            result = p;
        } else if (auto plist = dynamic_cast<DeclaratorParameterList *>(mod.get())) {
            auto p = std::make_shared<FunctionType>();
            for (auto &param : plist->parameters)
                p->parameters.push_back(create(param.specifiers, param.declarator, param.pos, scopes));
            
            p->returnType = result;
            result = p;
        } else
            // unknown modifier
            throw CompilerError("unknown modifier", mod->pos);
    }
    
    return result;
}

Ptr<Type> Type::add(Ptr<Type> &a, Ptr<Type> &b, lexer::TextPosition pos) {
    auto arA = dynamic_cast<ArithmeticType *>(a.get());
    auto arB = dynamic_cast<ArithmeticType *>(b.get());

    if (arA && arB) {
        auto size = ArithmeticType::max(arA->size, arB->size);
        auto sign = ArithmeticType::max(arA->sign, arB->sign);
        
        if (arA->size == size && arA->sign == sign) return a;
        if (arB->size == size && arB->sign == sign) return b;
        
        return std::make_shared<ArithmeticType>(sign, size);
    }
    
    auto ptrA = dynamic_cast<PointerType *>(a.get());
    auto ptrB = dynamic_cast<PointerType *>(b.get());
    
    if (arA && ptrB) return b;
    if (ptrA && arB) return a;
    
    throw CompilerError("cannot add incompatible types", pos);
}

Ptr<Type> Type::subtract(Ptr<Type> &a, Ptr<Type> &b, lexer::TextPosition pos) {
    auto arA = dynamic_cast<ArithmeticType *>(a.get());
    auto arB = dynamic_cast<ArithmeticType *>(b.get());

    if (arA && arB) { // @todo not DRY
        auto size = ArithmeticType::max(arA->size, arB->size);
        auto sign = ArithmeticType::max(arA->sign, arB->sign);
        
        if (arA->size == size && arA->sign == sign) return a;
        if (arB->size == size && arB->sign == sign) return b;
        
        return std::make_shared<ArithmeticType>(sign, size);
    }
    
    auto ptrA = dynamic_cast<PointerType *>(a.get());
    auto ptrB = dynamic_cast<PointerType *>(b.get());
    
    if (ptrA && arB) return a;
    
    if (ptrA && ptrB) {
        if (!ptrA->base->isCompatible(*ptrB->base))
            throw CompilerError("subtracting incompatible pointer types", pos);
        return Type::ptrdiffType;
    }
    
    throw CompilerError("cannot subtract incompatible types", pos);
}

bool Type::canCompare(const Type &a, const Type &b, bool broad) {
    if (broad && a.isCompatible(b))
        return true;
    
    auto arA = dynamic_cast<const ArithmeticType *>(&a);
    auto arB = dynamic_cast<const ArithmeticType *>(&b);
    if (arA && arB) return true;
    
    auto ptrA = dynamic_cast<const PointerType *>(&a);
    auto ptrB = dynamic_cast<const PointerType *>(&b);
    if (ptrA && ptrB) {
        if (ptrA->base->isCompatible(*ptrB->base)) return true;
        if (ptrA->isVoidPointer() || ptrB->isVoidPointer()) return true;
    }
    
    if (ptrA && b.isNullPointer()) return true;
    if (a.isNullPointer() && ptrB) return true;
    
    return false;
}

Ptr<Type> Type::ptrdiffType = std::make_shared<ArithmeticType>(ArithmeticType::UNSIGNED, ArithmeticType::INT);
std::set<Type *> Type::typeQueue = std::set<Type *>();

bool FunctionType::isCompatible(const Type &other) const {
    if (auto p = dynamic_cast<const PointerType *>(&other))
        return p->base->isCompatible(*this);
    
    if (auto f = dynamic_cast<const FunctionType *>(&other)) {
        if (parameters.size() != f->parameters.size())
            return false;
        
        for (size_t i = 0; i < parameters.size(); ++i)
            if (!parameters[i]->isCompatible(*f->parameters[i])) // @todo isEqual
                return false;
        
        return true;
    }
    
    return false;
}

bool PointerType::isCompatible(const Type &other) const {
    if (dynamic_cast<const NullPointerType *>(&other)) return true;
    if (dynamic_cast<const FunctionType *>(&other)) return other.isCompatible(*this);
    
    auto p = dynamic_cast<const PointerType *>(&other);
    if (!p) return false;
    return base->isCompatible(*p->base);
}

bool NullPointerType::isCompatible(const Type &other) const {
    return dynamic_cast<const ArithmeticType *>(&other) || dynamic_cast<const PointerType *>(&other);
}
