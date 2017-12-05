//
//  ASTInspector.h
//  c4
//
//  Created by Alexander Rath on 12.11.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#ifndef ASTINSPECTOR_h
#define ASTINSPECTOR_h

#include "AST.h"

using namespace ast;

class ASTInspector : public Visitor {
protected:
    std::string indent = "";
    
    void inspect(Node &node) {
        std::string prev_indent = indent;
        
        indent += "  ";
        node.accept(*this);
        
        indent = prev_indent;
    }
    
    template<typename T>
    void inspect(Ptr<T> &ptr) {
        inspect(*ptr);
    }
    
    template<typename T>
    void inspect_vector(std::string title, Vector<T> &vector) {
        std::string prev_indent = indent;
        
        std::cout << indent << "  " << title << std::endl;
        indent += "  ";
        
        for (auto &child : vector)
            inspect(child);
        
        indent = prev_indent;
    }
    
public:
    virtual void visit(CaseLabel &node) {
        std::cout << indent << "CaseLabel" << std::endl;
        inspect(*node.expression);
    }
    
    virtual void visit(DefaultLabel &) {
        std::cout << indent << "DefaultLabel" << std::endl;
    }
    
    virtual void visit(IdentifierLabel &) {
        std::cout << indent << "IdentifierLabel" << std::endl;
    }

    virtual void visit(Identifier &node) {
        std::cout << indent << "Identifier[" << node.id << "]" << std::endl;
    }

    virtual void visit(NamedTypeSpecifier &node) {
        std::cout << indent << "NamedTypeSpecifier[" << node.id << "]" << std::endl;
    }

    virtual void visit(CompoundStatement &node) {
        std::cout << indent << "CompoundStatement" << std::endl;
        inspect_vector("items", node.items);
    }
    
    virtual void visit(DeclaratorPointer &) {
        std::cout << indent << "DeclaratorPointer" << std::endl;
    }
    
    virtual void visit(DeclaratorParameterList &node) {
        std::cout << indent << "DeclaratorParameterList" << std::endl;
        inspect_vector("parameters", node.parameters);
    }
    
    virtual void visit(Declarator &node) {
        std::cout << indent << "Declarator[" << (node.name ? node.name : "(abstract)") << "]" << std::endl;
        if (node.initializer.get())
            inspect(node.initializer);
        inspect_vector("modifiers", node.modifiers);
    }

    virtual void visit(Declaration &node) {
        std::cout << indent << "Declaration" << std::endl;
        inspect_vector("declarators", node.declarators);
        inspect_vector("specifiers", node.specifiers);
    }

    virtual void visit(ExternalDeclarationVariable &node) { // @todo Isn't this equivalent to Declaration?
        std::cout << indent << "ExternalDeclarationVariable" << std::endl;
        inspect_vector("declarators", node.declarators);
        inspect_vector("specifiers", node.specifiers);
    }

    virtual void visit(ExternalDeclarationFunction &node) {
        std::cout << indent << "ExternalDeclarationFunction" << std::endl;
        inspect_vector("declarators", node.declarators);
        inspect_vector("specifiers", node.specifiers);
        inspect_vector("declarations", node.declarations);
        inspect(node.body);
    }

    virtual void visit(ParameterDeclaration &node) {
        std::cout << indent << "ParameterDeclaration" << std::endl;
        inspect_vector("specifiers", node.specifiers);
        inspect(node.declarator);
    }

    virtual void visit(ComposedTypeSpecifier &node) {
        std::cout << indent << "ComposedTypeSpecifier[" << (node.name ? node.name : "(unnamed)") << "]" << std::endl;
        inspect_vector("declarations", node.declarations);
    }

    virtual void visit(ConstantExpression &node) {
        std::cout << indent << "ConstantExpression[" << node.text << "]" << std::endl;
    }
    
    virtual void visit(CastExpression &node) {
        std::cout << indent << "CastExpression" << std::endl;
        inspect(node.type);
        inspect(node.expression);
    }

    virtual void visit(UnaryExpression &node) {
        std::cout << indent << "UnaryExpression[" << operator_name(node.op) << "]" << std::endl;
        inspect(node.operand);
    }

    virtual void visit(BinaryExpression &node) {
        std::cout << indent << "BinaryExpression[" << operator_name(node.op) << "]" << std::endl;
        inspect(node.lhs);
        inspect(node.rhs);
    }

    virtual void visit(ConditionalExpression &node) {
        std::cout << indent << "ConditionalExpression" << std::endl;
        inspect(node.condition);
        inspect(node.when_true);
        inspect(node.when_false);
    }

    virtual void visit(ExpressionList &node) {
        std::cout << indent << "ExpressionList" << std::endl;
        inspect_vector("children", node.children);
    }

    virtual void visit(CallExpression &node) {
        std::cout << indent << "CallExpression" << std::endl;
        inspect(node.function);
        inspect_vector("arguments", node.arguments);
    }
    
    virtual void visit(SubscriptExpression &node) {
        std::cout << indent << "SubscriptExpression" << std::endl;
        inspect(node.base);
        inspect(node.subscript);
    }
    
    virtual void visit(MemberExpression &node) {
        std::cout
            << indent
            << "MemberExpression[" << node.id << ", " << (node.dereference ? "->" : ".") << "]"
            << std::endl;
        
        inspect(node.base);
    }
    
    virtual void visit(PostExpression &node) {
        std::cout << indent << "PostExpression[" << operator_name(node.op) << "]" << std::endl;
        inspect(node.base);
    }

    virtual void visit(ExpressionStatement &node) {
        std::cout << indent << "ExpressionStatement" << std::endl;
        inspect_vector("labels", node.labels);
        inspect(node.expressions);
    }

    virtual void visit(SizeofExpressionUnary &node) {
        std::cout << indent << "SizeofExpressionUnary" << std::endl;
        inspect(node.expression);
    }

    virtual void visit(TypeName &node) {
        std::cout << indent << "TypeName" << std::endl;
        inspect_vector("specifiers", node.specifiers);
        inspect(node.declarator);
    }

    virtual void visit(SizeofExpressionTypeName &node) {
        std::cout << indent << "SizeofExpressionTypeName" << std::endl;
        inspect(node.type);
    }

    virtual void visit(DesignatorWithIdentifier &node) {
        std::cout << indent << "DesignatorWithIdentifier[" << node.id << "]" << std::endl;
    }

    virtual void visit(DesignatorWithExpression &node) {
        std::cout << indent << "DesignatorWithExpression" << std::endl;
        inspect(node.expression);
    }

    virtual void visit(Initializer &node) {
        std::cout << indent << "Initializer" << std::endl;
        inspect(node.declarator);
        inspect_vector("designators", node.designators);
    }

    virtual void visit(InitializerList &node) {
        std::cout << indent << "InitializerList" << std::endl;
        inspect_vector("initializers", node.initializers);
    }

    virtual void visit(InitializerExpression &node) {
        std::cout << indent << "InitializerExpression" << std::endl;
        inspect(node.type);
        inspect(node.initializers);
    }

    virtual void visit(IterationStatement &node) {
        std::cout << indent << "IterationStatement" << std::endl;
        inspect_vector("labels", node.labels);
        inspect(node.condition);
        inspect(node.body);
    }

    virtual void visit(SelectionStatement &node) {
        std::cout << indent << "SelectionStatement" << std::endl;
        inspect_vector("labels", node.labels);
        inspect(node.condition);
        inspect(node.when_true);
        
        if (node.when_false.get())
            inspect(node.when_false);
    }

    virtual void visit(GotoStatement &node) {
        std::cout << indent << "GotoStatement[" << node.target << "]" << std::endl;
        inspect_vector("labels", node.labels);
    }

    virtual void visit(ContinueStatement &node) {
        std::cout << indent << "ContinueStatement" << std::endl;
        inspect_vector("labels", node.labels);
    }

    virtual void visit(ReturnStatement &node) {
        std::cout << indent << "ReturnStatement" << std::endl;
        inspect_vector("labels", node.labels);
        inspect(node.expressions);
    }
};

#endif /* ASTInspector_h */
