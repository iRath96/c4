//
//  Parser.hpp
//  c4
//
//  Created by Alexander Rath on 04.11.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#ifndef Parser_hpp
#define Parser_hpp

#include <stdio.h>
#include <iostream>
#include <vector>
#include <memory>
#include <sstream>

#include "Lexer.h"

class Parser;

struct DebugTree {
public:
    const char *function = "(root)";
    int start_index = 0, end_index = 0;
    bool ret_val, has_returned = false;
    
    DebugTree *parent;
    std::vector<std::shared_ptr<DebugTree>> children;
    
    DebugTree *create_child(const char *function, int index) {
        auto child = std::make_shared<DebugTree>();
        child->function = function;
        child->start_index = index;
        child->parent = this;
        children.push_back(child);
        
        return child.get();
    }
    
    DebugTree *perform_return(bool value, int index) {
        has_returned = true;
        ret_val = value;
        end_index = index;
        
        return parent;
    }
    
    void dump(Parser *parser, std::string indent = "");
};

extern DebugTree dbg_tree_root;
extern DebugTree *dbg_tree_current;

#define DEBUG_HOOK {\
    dbg_tree_current = dbg_tree_current->create_child(__PRETTY_FUNCTION__, i);\
}

#define DEBUG_RETURN(x) {\
    dbg_tree_current = dbg_tree_current->perform_return(x, i);\
    return x;\
}

#define ACCEPT DEBUG_RETURN(true)
#define DENY DEBUG_RETURN(false)

#define NON_EMPTY(stmt, err) {\
    if (!(stmt)) \
        error(err); \
}

#define NON_EMPTY_RET(stmt) {\
    if (!(stmt)) \
        DEBUG_RETURN(false); \
}

#define OPTION { \
    DEBUG_HOOK \
    int _initial_i = i; \
    { \
        __label__ deny;

#define ELSE_OPTION \
        ACCEPT \
        deny: \
        i = _initial_i; \
    } \
    { \
        __label__ deny;

#define END_OPTION \
        ACCEPT \
        deny: \
        i = _initial_i; \
    } \
    DENY \
}

#define NON_OPTIONAL(stmt) \
    if (!(stmt)) goto deny;
#define OPTIONAL(stmt) \
    stmt;

const char *operator_name(TokenPunctuator punctuator);

class ParserError {
public:
    std::string message;
    TextPosition pos;
    
    ParserError(const std::string &message, TextPosition pos)
    : message(message), pos(pos) {
        
    }
};

class Node {
public:
    virtual void describe(std::ostream &s, std::string indent) {
        s << indent << "not implemented" << std::endl;
    }
};

class Expression : public Node {
public:
};

class Label : public Node {
public:
};

class NodeBlockItem : public Node {
public:
};

class Statement : public NodeBlockItem {
public:
    std::vector<std::shared_ptr<Label>> labels;
};

class CaseLabel : public Label {
public:
    std::shared_ptr<Expression> expression;
};

class DefaultLabel : public Label {
public:
};

class IdentifierLabel : public Label {
public:
    const char *id;
};

class CompoundStatement : public Statement {
public:
    std::vector<std::shared_ptr<NodeBlockItem>> items;
    
    virtual void describe(std::ostream &s, std::string indent) {
        s << indent << "CompoundStatement" << std::endl;
        for (auto &item : items)
            item->describe(s, indent + "  ");
    }
};

class NodeIdentifier : public Node {
public:
    const char *id;
    NodeIdentifier(const char *id) : id(id) {}
    
    virtual void describe(std::ostream &s, std::string indent) {
        s << "id(" << id << ")";
    }
};

class NodeTypeSpecifier : public Node {
public:
};

class NodeTypeNamed : public NodeTypeSpecifier {
public:
    const char *name;
    
    virtual void describe(std::ostream &s, std::string indent) {
        s << indent << name << std::endl;
    }
};

class NodePointer : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
};

class NodeDeclarator : public Node {
public:
    const char *name;
    std::vector<NodePointer> pointers;
    
    virtual void describe(std::ostream &s, std::string indent) {
        s << indent << "NodeDeclarator[" << name << ", " << pointers.size() << "]" << std::endl;
    }
};

class NodeDeclaration : public NodeBlockItem {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
    std::vector<NodeDeclarator> declarators;
    
    virtual void describe(std::ostream &s, std::string indent) {
        s << indent << "NodeDeclaration" << std::endl;
        
        s << indent << "  types:" << std::endl;
        for (auto &specifier : specifiers)
            specifier->describe(s, indent + "    ");
        
        s << indent << "  declarators:" << std::endl;
        for (auto &decl : declarators)
            decl.describe(s, indent + "    ");
    }
};

class NodeExternalDeclaration : public NodeDeclaration {
public:
};

class NodeExternalDeclarationVariable : public NodeExternalDeclaration {
public:
};

class NodeExternalDeclarationFunction : public NodeExternalDeclaration {
public:
    std::vector<NodeDeclaration> declarations;
    CompoundStatement body;
    
    virtual void describe(std::ostream &s, std::string indent) {
        NodeDeclaration::describe(s, indent);
        
        s << indent << "  declarations:" << std::endl;
        for (auto &decl : declarations)
            decl.describe(s, indent + "    ");
        
        body.describe(s, indent + "  ");
    }
};

class NodeParameterDeclaration : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
    NodeDeclarator declarator;
};

class NodeTypeComposed : public NodeTypeSpecifier {
public:
    TokenKeyword type;
    std::vector<NodeDeclaration> declarations;
    
    const char *name = NULL;
    
    virtual void describe(std::ostream &s, std::string indent) {
        s << indent << (type == TokenKeyword::STRUCT ? "struct" : "union") << std::endl;
        
        for (auto &decl : declarations)
            decl.describe(s, indent + "  ");
    }
};

class ExpressionConstant : public Expression {
public:
    const char *text;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "ExpressionConstant(" << text << ")" << std::endl;
    }
};

class ExpressionUnary : public Expression {
public:
    std::shared_ptr<Expression> operand;
    TokenPunctuator op;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "ExpressionUnary" << std::endl;
        std::cout << indent << "  " << operator_name(op) << std::endl;
        operand->describe(s, indent + "  ");
    }
};

class ExpressionBinary : public Expression {
public:
    std::shared_ptr<Expression> lhs, rhs;
    TokenPunctuator op;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "ExpressionBinary" << std::endl;
        lhs->describe(s, indent + "  ");
        std::cout << indent << "  " << operator_name(op) << std::endl;
        rhs->describe(s, indent + "  ");
    }
};

class ExpressionConditional : public Expression {
public:
    std::shared_ptr<Expression> condition, when_true, when_false;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "ExpressionConditional" << std::endl;
        condition->describe(s, indent + "  ");
        when_true->describe(s, indent + "  ");
        when_false->describe(s, indent + "  ");
    }
};

class ExpressionList : public Expression {
public:
    std::vector<std::shared_ptr<Expression>> children;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "ExpressionList" << std::endl;
        for (auto &child : children)
            child->describe(s, indent + "  ");
    }
};

class ExpressionCall : public Expression {
public:
    std::shared_ptr<Expression> function;
    std::vector<std::shared_ptr<Expression>> arguments;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "ExpressionCall" << std::endl;
        function->describe(s, indent + "  ");
        
        for (auto &arg : arguments) {
            arg->describe(s, indent + "  ");
        }
    }
};

class ExpressionStatement : public Statement {
public:
    ExpressionList expressions;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "ExpressionStatement" << std::endl;
        expressions.describe(s, indent + "  ");
    }
};

class SizeofExpression : public Expression {
public:
};

class SizeofExpressionUnary : public SizeofExpression {
public:
    std::shared_ptr<Expression> expression;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "SizeofExpressionUnary" << std::endl;
        expression->describe(s, indent + "  ");
    }
};

class TypeName : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
};

class SizeofExpressionTypeName : public SizeofExpression {
public:
    TypeName type;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "SizeofExpressionTypeName" << std::endl;
    }
};

class InitializerExpression : public Expression {
public:
    TypeName type;
    std::vector<NodeDeclarator> initializers;
    
    virtual void describe(std::ostream &s, std::string indent) {
        std::cout << indent << "InitializerExpression" << std::endl;
        type.describe(s, indent + "  ");
        
        for (auto &i : initializers) {
            i.describe(s, indent + "  ");
        }
    }
};

class Parser {
public:
    Parser(Lexer lexer) : lexer(lexer) {
    }
    
    void parse() {
        std::vector<std::shared_ptr<NodeExternalDeclaration>> declarations;
        read_list(&Parser::read_external_declaration, declarations);
        
        if (!eof())
            error("declaration expected");
        
        /*
        for (auto &decl : declarations) {
            decl->describe(std::cout, "");
        }
        */
        
        //dbg_tree_root.dump(this);
    }
    
    void print_context(int index = -1) {
        if (index < 0)
            index = i;
        
        for (int j = -(index >= 5 ? 5 : index); j <= 5; ++j) {
            if (!j)
                std::cout << "(here) ";
            std::cout << peek(index - i + j).text << " ";
        }
        
        std::cout << std::endl;
    }
    
    void print_debug_tree() {
        dbg_tree_root.dump(this);
    }
    
protected:
    int i;
    
    struct {
        int i = 0;
        std::vector<Token> tokens;
    } error_loc;
    
    bool shift(bool condition = true) {
        i += condition ? 1 : 0;
        return condition;
    }
    
    Lexer lexer;
    std::vector<Token> token_queue;
    
    bool eof(int offset = 0) {
        int i = this->i + offset;
        return (lexer.has_ended() && (int)token_queue.size() < i) || peek(offset).type == TokenType::END;
    }
    
    Token &peek(int offset = 0) {
        int i = this->i + offset;
        
        if (i < (int)token_queue.size())
            return token_queue[i];
        
        while (!lexer.has_ended() && (int)token_queue.size() <= i) {
            // printf("reading token...\n");
            token_queue.push_back(lexer.next_token());
        }
        
        return token_queue[token_queue.size() - 1]; // size() <= i+1
    }
    
    [[noreturn]] void error(const std::string &message, int offset = 0) {
        TextPosition start_pos = peek(error_loc.i - i).pos;
        throw ParserError(message, start_pos);
    }
    
#pragma mark - Terminals
    
    void report_error(Token expected_token) {
        if (i < error_loc.i)
            return;
        
        if (i > error_loc.i) {
            error_loc.i = i;
            error_loc.tokens.clear();
        }
        
        error_loc.tokens.push_back(expected_token);
    }
    
    bool read_punctuator(TokenPunctuator punctuator) {
        DEBUG_HOOK
        
        if (peek().punctuator != punctuator) {
            report_error((Token){ .type = TokenType::PUNCTUATOR, .punctuator = punctuator });
            DENY
        }
        
        shift();
        ACCEPT
    }
    
    bool read_keyword(TokenKeyword keyword) {
        DEBUG_HOOK
        
        if (peek().keyword != keyword) {
            report_error((Token){ .type = TokenType::KEYWORD, .keyword = keyword });
            DENY
        }
        
        shift();
        ACCEPT
    }
    
    bool read_unary_operator(TokenPunctuator &op) {
        DEBUG_HOOK
        
        switch (peek().punctuator) {
            case TokenPunctuator::BIT_AND:
            case TokenPunctuator::ASTERISK:
            case TokenPunctuator::PLUS:
            case TokenPunctuator::MINUS:
            case TokenPunctuator::BIT_NOT:
            case TokenPunctuator::LOG_NOT:
                op = peek().punctuator;
                shift();
                
                break;
                
            default:
                report_error((Token){ .type = TokenType::PUNCTUATOR, .punctuator = TokenPunctuator::LOG_NOT });
                DENY
        }
        
        ACCEPT
    }
    
    bool read_token(TokenType type, const char *&text) {
        if (peek().type != type) {
            report_error((Token){ .type = type });
            return false;
        }
        
        text = peek().text;
        return shift();
    }
    
    bool read_identifier(const char *&text)
    OPTION
        NON_OPTIONAL(read_token(TokenType::IDENTIFIER, text))
    END_OPTION
    
    bool read_constant(const char *&text)
    OPTION
        NON_OPTIONAL(read_token(TokenType::CONSTANT, text))
    END_OPTION
    
    bool read_string_literal(const char *&text)
    OPTION
        NON_OPTIONAL(read_token(TokenType::STRING_LITERAL, text))
    END_OPTION
    
#pragma mark - Other stuff
    
    bool read_type_specifier_keyword(NodeTypeNamed &node) {
        switch (peek().keyword) {
            case TokenKeyword::VOID:
            case TokenKeyword::CHAR:
            case TokenKeyword::SHORT:
            case TokenKeyword::INT:
            case TokenKeyword::LONG:
            case TokenKeyword::FLOAT:
            case TokenKeyword::DOUBLE:
            case TokenKeyword::SIGNED:
            case TokenKeyword::UNSIGNED:
            case TokenKeyword::_BOOL:
            case TokenKeyword::_COMPLEX:
                node.name = peek().text;
                return shift();
                
            default:
                return false;
        }
    }
    
    template<typename T>
    bool read_list(bool (Parser::*method)(T &node), std::vector<T> &result) {
        int initial_i = i;
        
        while (!eof()) {
            T temp;
            if (!(this->*method)(temp))
                break;
            
            result.push_back(temp);
        }
        
        return i != initial_i;
    }
    
    template<typename T>
    bool read_separated_list(bool (Parser::*method)(T &node), TokenPunctuator separator, std::vector<T> &result) {
        int initial_i = i;
        int last_good_i = i;
        
        while (!eof()) {
            T temp;
            if (!(this->*method)(temp))
                break;
            
            result.push_back(temp);
            last_good_i = i;
            
            if(!read_punctuator(separator))
                break;
        }
        
        i = last_good_i;
        return i != initial_i;
    }
    
#pragma mark - Declarations
    
    bool read_declarator(NodeDeclarator &node) {
        DEBUG_HOOK
        
        if (read_pointer(node))
            NON_EMPTY(read_direct_declarator(node), "direct declarator expected")
        else
            NON_EMPTY_RET(read_direct_declarator(node))
        
        ACCEPT
    }
    
    bool read_declaration_specifiers(std::vector<std::shared_ptr<NodeTypeSpecifier>> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
    END_OPTION
    
    bool read_direct_declarator_prefix(NodeDeclarator &node)
    OPTION
        NON_OPTIONAL(read_identifier(node.name))
    ELSE_OPTION
        NodeDeclarator node;
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_OPEN))
        NON_OPTIONAL(read_declarator(node))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_CLOSE))
    END_OPTION
    
    bool read_direct_declarator(NodeDeclarator &node) {
        DEBUG_HOOK
        
        NON_EMPTY_RET(read_direct_declarator_prefix(node));
        
        int last_good_i = i;
        
        while (!eof()) {
            // try reading parameter-list / identifier-list
            
            std::vector<NodeParameterDeclaration> parameter_list;
            std::vector<const char *> identifier_list;
            
            if (!read_punctuator(TokenPunctuator::RB_OPEN))
                break;
            
            if (read_parameter_type_list()) {
            } else if (read_identifier_list(identifier_list)) {
            }
            
            if (!read_punctuator(TokenPunctuator::RB_CLOSE))
                break;
            
            last_good_i = i;
        }
        
        i = last_good_i;
        ACCEPT
    }
    
    bool read_identifier_list(std::vector<const char *> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_identifier, TokenPunctuator::COMMA, node))
    END_OPTION
    
    bool read_type_name(TypeName &node)
    OPTION
        NON_OPTIONAL(read_specifier_qualifier_list(node.specifiers))
        OPTIONAL(read_abstract_declarator())
    END_OPTION
    
    bool read_abstract_declarator()
    OPTION
        NodeDeclarator ptr;
    
        if (read_pointer(ptr))
            OPTIONAL(read_direct_abstract_declarator())
        else
            NON_OPTIONAL(read_direct_abstract_declarator())
    END_OPTION
    
    bool read_direct_abstract_declarator() {
        DEBUG_HOOK
        
        int initial_i = i;
        
        if (read_punctuator(TokenPunctuator::RB_OPEN)) {
            NON_EMPTY(read_abstract_declarator(), "abstract declarator expected");
            NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), ") expected");
        }
        
        while (!eof()) {
            if (read_punctuator(TokenPunctuator::SB_OPEN)) {
                NON_EMPTY(read_punctuator(TokenPunctuator::ASTERISK), "* expected");
                NON_EMPTY(read_punctuator(TokenPunctuator::SB_CLOSE), "] expected");
            } else if (read_punctuator(TokenPunctuator::RB_OPEN)) {
                read_parameter_type_list();
                NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), ") expected");
            } else
                break;
        }
        
        DEBUG_RETURN(i != initial_i)
    }
    
    bool read_parameter_type_list()
    OPTION
        std::vector<NodeParameterDeclaration> pd;
        NON_OPTIONAL(read_parameter_list(pd))
    END_OPTION
    
    bool read_parameter_list(std::vector<NodeParameterDeclaration> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_parameter_declaration, TokenPunctuator::COMMA, node))
    END_OPTION
    
    bool read_parameter_declaration(NodeParameterDeclaration &node)
    OPTION
        NON_OPTIONAL(read_declaration_specifiers(node.specifiers))
        if (read_declarator(node.declarator)) {
        } else if (read_abstract_declarator()) {
        }
    END_OPTION
    
    bool read_pointer_single(NodePointer &node)
    OPTION
        NON_OPTIONAL(read_punctuator(TokenPunctuator::ASTERISK))
        OPTIONAL(read_type_qualifier_list(node.specifiers))
    END_OPTION
    
    bool read_pointer(NodeDeclarator &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_pointer_single, node.pointers))
    END_OPTION
    
    bool read_type_qualifier_list(std::vector<std::shared_ptr<NodeTypeSpecifier>> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
    END_OPTION
    
    bool read_specifier_qualifier_list(std::vector<std::shared_ptr<NodeTypeSpecifier>> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
    END_OPTION
    
#pragma mark - Structs
    
    bool read_struct_declarator(NodeDeclarator &node)
    OPTION
        NON_OPTIONAL(read_declarator(node))
    END_OPTION
    
    bool read_struct_declarator_list(std::vector<NodeDeclarator> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_struct_declarator, TokenPunctuator::COMMA, node))
    END_OPTION
    
    bool read_struct_declaration(NodeDeclaration &node)
    OPTION
        NON_OPTIONAL(read_specifier_qualifier_list(node.specifiers))
        OPTIONAL(read_struct_declarator_list(node.declarators))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::SEMICOLON))
    END_OPTION
    
    bool read_struct_declaration_list(std::vector<NodeDeclaration> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_struct_declaration, node))
    END_OPTION
    
    bool read_struct_body(NodeTypeComposed &node)
    OPTION
        NON_OPTIONAL(read_punctuator(TokenPunctuator::CB_OPEN))
        NON_OPTIONAL(read_struct_declaration_list(node.declarations))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::CB_CLOSE))
    END_OPTION
    
    bool read_type_specifier(std::shared_ptr<NodeTypeSpecifier> &node) {
        DEBUG_HOOK
        
        NodeTypeNamed type_name;
        if (read_type_specifier_keyword(type_name)) {
            node = std::make_shared<NodeTypeNamed>(type_name);
            ACCEPT
        }
        
        Token &token = peek();
        switch (token.keyword) {
            case TokenKeyword::STRUCT:
            case TokenKeyword::UNION: {
                shift();
                
                NodeTypeComposed *n = new NodeTypeComposed();
                node.reset(n);
                
                n->type = token.keyword;
                
                bool has_identifier = read_identifier(n->name);
                bool has_body = peek().punctuator == TokenPunctuator::CB_OPEN;
                
                if (!has_body && !has_identifier)
                    error("struct/union without identifier or body");
                
                if (has_body)
                    NON_EMPTY(read_struct_body(*n), "struct/union body expected");
                
                break;
            }
                
            default:
                DENY
        }
        
        ACCEPT
    }
    
    bool read_initializer(NodeDeclarator &node)
    OPTION
        std::vector<NodeDeclarator> initializer_list;
    
        NON_OPTIONAL(read_punctuator(TokenPunctuator::CB_OPEN))
        NON_OPTIONAL(read_initializer_list(initializer_list))
        OPTIONAL(read_punctuator(TokenPunctuator::COMMA))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::CB_CLOSE))
    ELSE_OPTION
        std::shared_ptr<Expression> assignment_expr;
        NON_OPTIONAL(read_assignment_expression(assignment_expr))
    END_OPTION
    
    bool read_designation_initializer_pair(NodeDeclarator &node)
    OPTION
        if (read_designation()) {
            NON_OPTIONAL(read_initializer(node))
        } else {
            OPTIONAL(read_initializer(node))
        }
    END_OPTION
    
    bool read_initializer_list(std::vector<NodeDeclarator> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_designation_initializer_pair, TokenPunctuator::COMMA, node))
    END_OPTION
    
    bool read_designation()
    OPTION
        NON_OPTIONAL(read_designator_list())
        NON_OPTIONAL(read_punctuator(TokenPunctuator::ASSIGN))
    END_OPTION
    
    bool read_designator_list()
    OPTION
        std::vector<int> node;
        NON_OPTIONAL(read_list(&Parser::read_designator, node))
    END_OPTION
    
    bool read_designator(int &node)
    OPTION
        std::shared_ptr<Expression> expr;
    
        NON_OPTIONAL(read_punctuator(TokenPunctuator::SB_OPEN))
        NON_OPTIONAL(read_constant_expression(expr))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::SB_CLOSE))
    ELSE_OPTION
        const char *id;
    
        NON_OPTIONAL(read_punctuator(TokenPunctuator::PERIOD))
        NON_OPTIONAL(read_identifier(id))
    END_OPTION
    
    bool read_init_declarator(NodeDeclarator &node)
    OPTION
        NON_OPTIONAL(read_declarator(node))
        
        if (read_punctuator(TokenPunctuator::ASSIGN)) {
            // also read initializer
            NON_OPTIONAL(read_initializer(node));
        }
    END_OPTION
    
    bool read_init_declarator_list(std::vector<NodeDeclarator> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_init_declarator, TokenPunctuator::COMMA, node))
    END_OPTION
    
    bool read_declaration(NodeDeclaration &node)
    OPTION
        NON_OPTIONAL(read_declaration_specifiers(node.specifiers))
        OPTIONAL(read_init_declarator_list(node.declarators))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::SEMICOLON))
    END_OPTION
    
    bool read_declaration_list(std::vector<NodeDeclaration> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_declaration, node))
    END_OPTION
    
    bool read_external_declaration(std::shared_ptr<NodeExternalDeclaration> &node) {
        DEBUG_HOOK
        
        std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
        NodeDeclarator declarator;
        
        NON_EMPTY_RET(read_declaration_specifiers(specifiers));
        NON_EMPTY(read_declarator(declarator), "declarator expected");
        
        bool needs_declaration_list = peek().punctuator == TokenPunctuator::COMMA;
        bool needs_initialization = peek().punctuator == TokenPunctuator::ASSIGN;
        bool is_declaration = needs_initialization || needs_declaration_list || peek().punctuator == TokenPunctuator::SEMICOLON;
        
        if (is_declaration) {
            // declaration: ... (',' init-declarator-list(opt))(opt) ;
            
            auto n = new NodeExternalDeclarationVariable();
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            
            if (needs_initialization) {
                shift(); // consume assign
                read_initializer(declarator);
                
                needs_initialization = peek().punctuator == TokenPunctuator::COMMA;
            }
            
            node->declarators.push_back(std::move(declarator));
            
            if (needs_declaration_list) {
                shift(); // jump over comma
                read_init_declarator_list(node->declarators);
            }
            
            NON_EMPTY(read_punctuator(TokenPunctuator::SEMICOLON), "semicolon expected");
        } else {
            // function definition: ... declaration-list(opt) compound-statement
            
            auto n = new NodeExternalDeclarationFunction;
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            node->declarators.push_back(std::move(declarator));
            
            read_declaration_list(n->declarations);
            NON_EMPTY(read_compound_statement(n->body), "compound statement expected");
        }
        
        ACCEPT
    }
    
#pragma mark - Statements
    
    bool read_statement(std::shared_ptr<Statement> &node)
    OPTION
        NON_OPTIONAL(read_labeled_statement(node))
    ELSE_OPTION
        auto c = std::make_shared<CompoundStatement>();
        NON_OPTIONAL(read_compound_statement(*c))
        node = c;
    ELSE_OPTION
        auto e = std::make_shared<ExpressionStatement>();
        NON_OPTIONAL(read_expression_statement(*e))
        node = e;
    ELSE_OPTION
        NON_OPTIONAL(read_selection_statement())
    ELSE_OPTION
        NON_OPTIONAL(read_iteration_statement())
    ELSE_OPTION
        NON_OPTIONAL(read_jump_statement())
    END_OPTION
    
    bool read_labeled_statement(std::shared_ptr<Statement> &node)
    OPTION
        std::shared_ptr<Label> label;
    
        if (read_keyword(TokenKeyword::CASE)) {
            auto n = std::make_shared<CaseLabel>();
            NON_OPTIONAL(read_constant_expression(n->expression))
            label = n;
        } else if (read_keyword(TokenKeyword::DEFAULT)) {
            label = std::make_shared<DefaultLabel>();
        } else if (peek(0).type == TokenType::IDENTIFIER) {
            auto n = std::make_shared<IdentifierLabel>();
            read_identifier(n->id);
            label = n;
        } else
            DENY
        
        NON_OPTIONAL(read_punctuator(TokenPunctuator::COLON))
        NON_OPTIONAL(read_statement(node))
        
        node->labels.push_back(label);
    END_OPTION
    
    bool read_block_item(std::shared_ptr<NodeBlockItem> &node)
    OPTION
        auto n = std::make_shared<NodeDeclaration>();
        NON_OPTIONAL(read_declaration(*n))
        node = n;
    ELSE_OPTION
        std::shared_ptr<Statement> stmt;
        NON_OPTIONAL(read_statement(stmt))
        node = stmt;
    END_OPTION
    
    bool read_block_item_list(std::vector<std::shared_ptr<NodeBlockItem>> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_block_item, node))
    END_OPTION
    
    bool read_compound_statement(CompoundStatement &node)
    OPTION
        NON_OPTIONAL(read_punctuator(TokenPunctuator::CB_OPEN))
        OPTIONAL(read_block_item_list(node.items))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::CB_CLOSE))
    END_OPTION
    
    bool read_expression_statement(ExpressionStatement &node)
    OPTION
        NON_OPTIONAL(read_expression(node.expressions))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::SEMICOLON))
    END_OPTION
    
    bool read_selection_statement()
    OPTION
        ExpressionList condition;
        std::shared_ptr<Statement> when_true, when_false;
        
        NON_OPTIONAL(read_keyword(TokenKeyword::IF))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_OPEN))
        NON_OPTIONAL(read_expression(condition))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_CLOSE))
        NON_OPTIONAL(read_statement(when_true))
        
        if (read_keyword(TokenKeyword::ELSE))
            NON_OPTIONAL(read_statement(when_false))
    END_OPTION
    
    bool read_iteration_statement()
    OPTION
        ExpressionList condition;
        std::shared_ptr<Statement> body;
        
        NON_OPTIONAL(read_keyword(TokenKeyword::WHILE))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_OPEN))
        NON_OPTIONAL(read_expression(condition))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_CLOSE))
        NON_OPTIONAL(read_statement(body))
    END_OPTION
    
    bool read_jump_statement()
    OPTION
        const char *target;
    
        NON_OPTIONAL(read_keyword(TokenKeyword::GOTO))
        NON_OPTIONAL(read_identifier(target))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::SEMICOLON))
    ELSE_OPTION
        NON_OPTIONAL(read_keyword(TokenKeyword::CONTINUE) || read_keyword(TokenKeyword::BREAK))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::SEMICOLON))
    ELSE_OPTION
        ExpressionList list;
    
        NON_OPTIONAL(read_keyword(TokenKeyword::RETURN))
        OPTIONAL(read_expression(list))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::SEMICOLON))
    END_OPTION
    
#pragma mark - Expressions
    
    bool read_expression(ExpressionList &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_assignment_expression, TokenPunctuator::COMMA, node.children))
    END_OPTION
    
    bool read_primary_expression(std::shared_ptr<Expression> &node)
    OPTION
        auto constant = std::make_shared<ExpressionConstant>();
        NON_OPTIONAL(read_identifier(constant->text) || read_constant(constant->text) || read_string_literal(constant->text))
        node = constant;
    ELSE_OPTION
        auto list = std::make_shared<ExpressionList>();
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_OPEN))
        NON_OPTIONAL(read_expression(*list))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_CLOSE))
        node = list;
    END_OPTION
    
    bool read_argument_expression_list(std::vector<std::shared_ptr<Expression>> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_assignment_expression, TokenPunctuator::COMMA, node))
    END_OPTION
    
    bool read_postfix_expression_prefix(std::shared_ptr<Expression> &node)
    OPTION
        NON_OPTIONAL(read_primary_expression(node))
    ELSE_OPTION
        auto initializer = std::make_shared<InitializerExpression>();

        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_OPEN))
    
        NON_OPTIONAL(read_type_name(initializer->type))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_CLOSE))

        NON_OPTIONAL(read_punctuator(TokenPunctuator::CB_OPEN))
        NON_OPTIONAL(read_initializer_list(initializer->initializers))
        OPTIONAL(read_punctuator(TokenPunctuator::COMMA))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::CB_CLOSE))
    
        node = initializer;
    END_OPTION
    
    bool read_postfix_expression(std::shared_ptr<Expression> &node) {
        DEBUG_HOOK
        
        NON_EMPTY_RET(read_postfix_expression_prefix(node))
        
        while (!eof()) {
            if (read_punctuator(TokenPunctuator::SB_OPEN)) {
                ExpressionList list;
                NON_EMPTY(read_expression(list), "expression expected");
                NON_EMPTY(read_punctuator(TokenPunctuator::SB_CLOSE), "] expected");
            } else if (read_punctuator(TokenPunctuator::RB_OPEN)) {
                auto n = std::make_shared<ExpressionCall>();
                n->function = node;
                
                read_argument_expression_list(n->arguments);
                NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), ") expected");
                
                node = n;
            } else if (read_punctuator(TokenPunctuator::PERIOD)) {
                const char *id;
                NON_EMPTY(read_identifier(id), "subscript expected");
            } else if (read_punctuator(TokenPunctuator::ARROW)) {
                const char *id;
                NON_EMPTY(read_identifier(id), "subscript expected");
            } else if (read_punctuator(TokenPunctuator::PLUSPLUS)) {
            } else if (read_punctuator(TokenPunctuator::MINUSMINUS)) {
            } else
                break;
        }
        
        ACCEPT
    }
    
    bool read_unary_expression(std::shared_ptr<Expression> &node)
    OPTION
        auto unary_node = std::make_shared<ExpressionUnary>();
        NON_OPTIONAL(read_punctuator(TokenPunctuator::PLUSPLUS) || read_punctuator(TokenPunctuator::MINUSMINUS))
        NON_OPTIONAL(read_unary_expression(unary_node->operand))
        node = unary_node;
    ELSE_OPTION
        auto unary_node = std::make_shared<ExpressionUnary>();
        NON_OPTIONAL(read_unary_operator(unary_node->op))
        NON_OPTIONAL(read_cast_expression(unary_node->operand))
        node = unary_node;
    ELSE_OPTION
        std::shared_ptr<Expression> u;
    
        NON_OPTIONAL(read_keyword(TokenKeyword::SIZEOF))
    
        if (read_unary_expression(u)) {
            auto s = std::make_shared<SizeofExpressionUnary>();
            s->expression = u;
            node = s;
        } else if (read_punctuator(TokenPunctuator::RB_OPEN)) {
            auto s = std::make_shared<SizeofExpressionTypeName>();
            NON_OPTIONAL(read_type_name(s->type))
            NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_CLOSE))
            node = s;
        }
    ELSE_OPTION
        TypeName type_name;
    
        NON_OPTIONAL(read_keyword(TokenKeyword::_ALIGNOF))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_OPEN))
        NON_OPTIONAL(read_type_name(type_name))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_CLOSE))
    ELSE_OPTION
        NON_OPTIONAL(read_postfix_expression(node))
    END_OPTION
    
    bool read_cast_expression(std::shared_ptr<Expression> &node)
    OPTION
        TypeName type_name;
    
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_OPEN))
        NON_OPTIONAL(read_type_name(type_name))
        NON_OPTIONAL(read_punctuator(TokenPunctuator::RB_CLOSE))
    
        NON_OPTIONAL(read_cast_expression(node))
    ELSE_OPTION
        NON_OPTIONAL(read_unary_expression(node))
    END_OPTION
    
    bool read_expression_with_precedence(Precedence left_precedence, std::shared_ptr<Expression> &node) {
        DEBUG_HOOK
        
        std::shared_ptr<Expression> root;
        NON_EMPTY_RET(read_cast_expression(root));
        
        while (!eof()) {
            TokenPunctuator op = peek().punctuator;
            Precedence right_precedence = PRECEDENCE(op);
            
            if (op == TokenPunctuator::QMARK ?
                (right_precedence == left_precedence) : // right-assoc
                (right_precedence >= left_precedence)   // left-assoc
                )
                break;
            
            if (peek().punctuator == TokenPunctuator::QMARK) {
                // conditional operator
                
                auto tree = std::make_shared<ExpressionConditional>();
                tree->condition = root;
                root = tree;
                
                shift();
                
                auto elist = std::make_shared<ExpressionList>();
                tree->when_true = elist;
                
                NON_EMPTY(read_expression(*elist), "expression expected");
                NON_EMPTY(read_punctuator(TokenPunctuator::COLON), "colon expected");
                NON_EMPTY(read_expression_with_precedence((Precedence)(right_precedence - 1), tree->when_false), "expression expected");
            } else {
                // ordinary operator
                
                auto tree = std::make_shared<ExpressionBinary>();
                tree->op = peek().punctuator;
                tree->lhs = root;
                root = tree;
                
                shift();
                
                NON_EMPTY(read_expression_with_precedence(right_precedence, tree->rhs), "expression expected");
            }
        }
        
        node = root;
        
        ACCEPT
    }
    
    bool read_assignment_expression(std::shared_ptr<Expression> &node)
    OPTION
        NON_OPTIONAL(read_expression_with_precedence(Precedence::NONE, node))
    END_OPTION
    
    bool read_constant_expression(std::shared_ptr<Expression> &node)
    OPTION
        NON_OPTIONAL(read_expression_with_precedence(Precedence::ASSIGNMENT, node))
    END_OPTION
};

#undef NON_EMPTY
#undef NON_EMPTY_RET

#endif /* Parser_hpp */

