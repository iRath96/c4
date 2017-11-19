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
            indent += "  ";
        
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
    virtual void visit(ContinueStatement &) { std::cout << "continue;"; }

    virtual void visit(CompoundStatement &node) {
        join(node.labels, " ", " ");
        
        std::cout << "{";
        separate_lines(node.items);
        std::cout << "}";
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

    virtual void visit(Declarator &node) {
        join(node.pointers, "");
        if (node.name)
            std::cout << node.name;
        join(node.suffixes, "");
        
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
        join(node.declarators, ", ", " ");
        separate_lines(node.declarations, false);
        inspect(node.body);
    }

    virtual void visit(ParameterDeclaration &node) {
        join(node.specifiers, " ", " ");
        inspect(node.declarator);
    }

    virtual void visit(ConstantExpression &node) {
        std::cout << node.text;
    }

    virtual void visit(UnaryExpression &node) {
        std::cout << operator_name(node.op);
        std::cout << "(";
        inspect(node.operand);
        std::cout << ")";
    }

    virtual void visit(BinaryExpression &node) {
        inspect(node.lhs);
        std::cout << " " << operator_name(node.op) << " "; // @todo precedence!
        inspect(node.rhs);
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
        inspect(node.function);
        std::cout << "(";
        join(node.arguments, ", ");
        std::cout << ")";
    }
    
    virtual void visit(SubscriptExpression &node) {
        inspect(node.base);
        std::cout << "[";
        inspect(node.subscript);
        std::cout << "]";
    }
    
    virtual void visit(MemberExpression &node) {
        inspect(node.base);
        std::cout << (node.dereference ? "->" : ".") << node.id;
    }
    
    virtual void visit(PostExpression &node) {
        inspect(node.base);
        std::cout << operator_name(node.op);
    }

    virtual void visit(ExpressionStatement &node) {
        join(node.labels, " ", " ");
        inspect(node.expressions);
        std::cout << ";";
    }

    virtual void visit(SizeofExpressionUnary &node) {
        std::cout << "sizeof";
        inspect(node.expression);
    }

    virtual void visit(TypeName &node) {
        join(node.specifiers, " ");
    }
    
    virtual void visit(ComposedType &node) {
        std::cout << "struct";
        if (node.name)
            std::cout << " " << node.name;
        
        if (!node.declarations.empty()) {
            std::cout << " {";
            separate_lines(node.declarations);
            std::cout << "}";
        }
    }

    virtual void visit(SizeofExpressionTypeName &node) {
        std::cout << "sizeof(";
        inspect(node.type);
        std::cout << ")";
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
        inspect(node.declarator.initializer);
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
        join(node.labels, " ", " ");
        
        std::cout << "while (";
        inspect(node.condition);
        std::cout << ") ";
        inspect(node.body);
    }

    virtual void visit(SelectionStatement &node) {
        join(node.labels, " ", " ");
        
        std::cout << "if (";
        inspect(node.condition);
        std::cout << ") ";
        
        inspect(node.when_true);
        
        if (node.when_false.get()) {
            std::cout << " else ";
            inspect(node.when_false);
        }
    }

    virtual void visit(GotoStatement &node) {
        join(node.labels, " ", " ");
        std::cout << "goto " << node.target << ";";
    }

    virtual void visit(ReturnStatement &node) {
        std::cout << "return ";
        inspect(node.expressions);
        std::cout << ";";
    }
};

#endif /* Beautifier_h */
