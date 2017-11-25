//
//  Beautifier.h
//  c4
//
//  Created by Alexander Rath on 12.11.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#ifndef Beautifier_h
#define Beautifier_h

#include "AST.h"

using namespace ast;

class Beautifier : public Visitor {
protected:
    std::string indent = "";
    
    void inspect(Node &node) {
        node.accept(*this);
    }
    
    void inspect(const char *str) {
        std::cout << str;
    }
    
    template<typename T>
    void inspect(Ptr<T> &ptr) {
        inspect(*ptr);
    }
    
    template<typename T>
    void join(Vector<T> &vector, std::string delimiter, std::string suffix = "") {
        bool first = true;
        
        for (auto &child : vector) {
            if (first)
                first = false;
            else
                std::cout << delimiter;
            
            inspect(child);
        }
        
        if (!vector.empty())
            std::cout << suffix;
    }
    
    template<typename T>
    void separate_lines(Vector<T> &vector, bool do_indent = true) {
        std::string prev_indent = indent;
        if (do_indent)
            indent += "\t";
        
        for (auto &child : vector) {
            std::cout << std::endl << indent;
            inspect(child);
        }
        
        indent = prev_indent;
        
        std::cout << std::endl << indent;
    }
    
public:
    virtual void visit(CaseLabel &node) {
        std::cout << "case ";
        inspect(*node.expression);
        std::cout << ":";
    }
    
    virtual void visit(DefaultLabel &) { std::cout << "default:"; }
    virtual void visit(IdentifierLabel &node) { std::cout << node.id << ":"; }
    virtual void visit(Identifier &node) { std::cout << node.id; }
    virtual void visit(NamedType &node) { std::cout << node.id; }
    virtual void visit(Pointer &) { std::cout << "*"; }
    virtual void visit(ContinueStatement &node) {
        if (node.keyword == lexer::Token::Keyword::CONTINUE)
            std::cout << "continue;";
        else
            std::cout << "break;";
    }

    virtual void visit(CompoundStatement &node) {
        std::cout << "{";
        
        std::string prev_indent = indent;
        indent += "\t";
        
        for (auto &child : node.items) {
            std::cout << std::endl;
            
            Statement *stmt;
            if ((stmt = dynamic_cast<Statement *>(child.get()))) {
                join(stmt->labels, "\n", "\n");
            }
            
            std::cout << indent;
            inspect(child);
        }
        
        indent = prev_indent;
        
        std::cout << std::endl << indent << "}";
    }
    
    virtual void visit(DeclaratorParameterList &node) {
        std::cout << "(";
        join(node.parameters, ", ");
        std::cout << ")";
    }
    
    virtual void visit(DeclaratorIdentifierList &node) {
        std::cout << "(";
        join(node.identifiers, ", ");
        std::cout << ")";
    }

    virtual void visit(IdentifierDeclarator &node) {
        for (int i = 0; i < node.suffixes.size(); ++i)
            std::cout << "(";
        
        for (int i = 0; i < node.pointers.size(); ++i)
            std::cout << "(*";
        
        if (node.name)
            std::cout << node.name;
        
        for (int i = 0; i < node.pointers.size(); ++i)
            std::cout << ")";
        
        join(node.suffixes, ")", ")");
        
        if (node.initializer.get()) {
            std::cout << " = ";
            inspect(node.initializer);
        }
    }
    
    virtual void visit(ComposedDeclarator &node) {
        for (int i = 0; i < node.suffixes.size(); ++i)
            std::cout << "(";
        
        for (int i = 0; i < node.pointers.size(); ++i)
            std::cout << "(*";
        
        inspect(node.base);
        
        for (int i = 0; i < node.pointers.size(); ++i)
            std::cout << ")";
        
        join(node.suffixes, ")", ")");
        
        if (node.initializer.get()) {
            std::cout << " = ";
            inspect(node.initializer);
        }
    }

    virtual void visit(Declaration &node) {
        join(node.specifiers, " ", " ");
        join(node.declarators, " ");
        std::cout << ";";
    }

    virtual void visit(ExternalDeclarationVariable &node) {
        join(node.specifiers, " ", " ");
        join(node.declarators, ", ");
        std::cout << ";" << std::endl;
    }

    virtual void visit(ExternalDeclarationFunction &node) {
        join(node.specifiers, " ", " ");
        join(node.declarators, ", ");
        separate_lines(node.declarations, false);
        inspect(node.body);
        std::cout << std::endl;
    }

    virtual void visit(ParameterDeclaration &node) {
        join(node.specifiers, " ", node.declarator.get() ? " " : "");
        if (node.declarator.get())
            inspect(node.declarator);
    }

    virtual void visit(ConstantExpression &node) {
        std::cout << node.text;
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
        inspect(node.condition);
        std::cout << " ? ";
        inspect(node.when_true);
        std::cout << " : ";
        inspect(node.when_false);
    }

    virtual void visit(ExpressionList &node) {
        join(node.children, ", ");
    }

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
        inspect(node.base);
        std::cout << operator_name(node.op);
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
        join(node.specifiers, " ");
    }
    
    virtual void visit(ComposedType &node) {
        std::cout << "struct";
        if (node.name)
            std::cout << " " << node.name;
        
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

    virtual void visit(DesignatorWithIdentifier &node) {
        std::cout << "." << node.id;
    }

    virtual void visit(DesignatorWithExpression &node) {
        std::cout << "[";
        inspect(node.expression);
        std::cout << "]";
    }

    virtual void visit(Initializer &node) {
        join(node.designators, ", ");
        if (!node.designators.empty()) {
            std::cout << " = ";
        }
        inspect(node.declarator->initializer);
    }

    virtual void visit(InitializerList &node) {
        std::cout << "{ ";
        join(node.initializers, ", ", " ");
        std::cout << "}";
    }

    virtual void visit(InitializerExpression &node) {
        std::cout << "(";
        inspect(node.type);
        std::cout << ") ";
        inspect(node.initializers);
    }

    virtual void visit(IterationStatement &node) {
        std::cout << "while (";
        inspect(node.condition);
        std::cout << ")";
        
        inline_inspect(node.body.get());
    }
    
    static bool inline_if(Statement *node) {
        return !(dynamic_cast<ExpressionStatement *>(node)
            || dynamic_cast<ReturnStatement *>(node)
            || dynamic_cast<JumpStatement *>(node)
        );
    }
    
    void inline_inspect(Statement *node, bool suffix = false) {
        if (inline_if(node))
            std::cout << " ";
        else {
            std::cout << std::endl;
            join(node->labels, "\n", "\n");
            std::cout << indent << "\t";
        }
        
        inspect(*node);
        
        if (suffix) {
            if (inline_if(node))
                std::cout << " ";
            else
                std::cout << std::endl << indent;
        }
    }

    virtual void visit(SelectionStatement &node) {
        std::cout << "if (";
        inspect(node.condition);
        std::cout << ")";
        
        inline_inspect(node.when_true.get(), " ");
        
        if (node.when_false.get()) {
            std::cout << "else";
            
            inline_inspect(node.when_false.get());
        }
    }

    virtual void visit(GotoStatement &node) {
        std::cout << "goto " << node.target << ";";
    }

    virtual void visit(ReturnStatement &node) {
        std::cout << "return ";
        inspect(node.expressions);
        std::cout << ";";
    }
};

#endif /* Beautifier_h */
