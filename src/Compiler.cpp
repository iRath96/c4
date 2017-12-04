//
//  Compiler.cpp
//  c4
//
//  Created by Alexander Rath on 04.12.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include <stdio.h>
#include "Compiler.h"

Ptr<Type> Type::reference() {
    return std::make_shared<PointerType>(shared_from_this());
}

Ptr<Type> Type::create(const PtrVector<TypeSpecifier> &specifiers, lexer::TextPosition pos) {
    using Keyword = lexer::Token::Keyword;
    const auto NAK = Keyword::NOT_A_KEYWORD;

    if (specifiers.empty())
        // invalid type
        throw CompilerError("no type specifiers", pos);

    Keyword size = Keyword::NOT_A_KEYWORD, sign = Keyword::NOT_A_KEYWORD;
    ast::ComposedType *comp = NULL;

    for (auto &spec : specifiers) {
        if (auto nt = dynamic_cast<NamedType *>(spec.get())) {
            switch (nt->keyword) {
            case Keyword::CHAR:
            case Keyword::INT:
                if (size != NAK)
                    throw CompilerError("multiple sizes specified", spec->pos);
                size = nt->keyword;
                break;
            
            case Keyword::SIGNED:
            case Keyword::UNSIGNED:
                if (sign != NAK)
                    throw CompilerError("multiple signednesses specified", spec->pos);
                sign = nt->keyword;
                break;
                
            case Keyword::VOID:
                if (specifiers.size() != 1)
                    throw CompilerError("void conflict", spec->pos);
                return std::make_shared<VoidType>();
                
            default:
                throw CompilerError("unknown type", spec->pos);
            }
        } else if (auto ct = dynamic_cast<ast::ComposedType *>(spec.get())) {
            if (comp)
                // multiple composed types
                throw CompilerError("multiple composed types specified", spec->pos);
            comp = ct;
        } else
            // unknown AST type
            throw CompilerError("unimplemented", spec->pos);
    }

    bool isArithmetic = size != NAK || sign != NAK;
    bool isComposed = comp;

    if (isComposed && isArithmetic)
        // conflicting type
        throw CompilerError("conflicting type", pos);

    if (isArithmetic) {
        auto s = std::make_shared<ArithmeticType>();
        // @todo
        return s;
    } else {
        auto c = std::make_shared<::ComposedType>();
        for (auto &declaration : comp->declarations) {
            Ptr<Type> type = Type::create(declaration.specifiers, declaration.pos);
            for (auto &decl : declaration.declarators)
                c->addMember(std::string(decl.name), type->applyDeclarator(decl), decl.pos);
        }
        return c;
    }
}

Ptr<Type> Type::create(const PtrVector<TypeSpecifier> &specifiers, const Declarator &decl, lexer::TextPosition pos) {
    return create(specifiers, pos)->applyDeclarator(decl);
}

Ptr<Type> Type::applyDeclarator(Declarator decl) {
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
                p->parameters.push_back(create(param.specifiers, param.declarator, param.pos));
            
            p->returnType = result;
            result = p;
        } else
            // unknown modifier
            throw CompilerError("unknown modifier", mod->pos);
    }
    
    return result;
}
