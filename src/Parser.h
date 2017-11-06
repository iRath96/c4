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

#include "Lexer.h"

#define NON_EMPTY(stmt, err) {\
    if (!stmt) \
        error(err); \
}

#define NON_EMPTY_RET(stmt) {\
    if (!stmt) \
        return false; \
}

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
};

class NodeIdentifier : public Node {
public:
    const char *id;
    NodeIdentifier(const char *id) : id(id) {}
};

class NodeTypeSpecifier : public Node {
public:
};

class NodeTypeNamed : public NodeTypeSpecifier {
public:
    const char *name;
};

class NodePointer : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
};

class NodeDeclarator : public Node {
public:
    const char *name;
    std::vector<NodePointer> pointers;
};

class NodeDeclaration : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
    std::vector<NodeDeclarator> declarators;
};

class NodeExternalDeclaration : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
    std::vector<NodeDeclarator> declarators;
};

class NodeExternalDeclarationVariable : public NodeExternalDeclaration {
public:
};

class NodeExternalDeclarationFunction : public NodeExternalDeclaration {
public:
    std::vector<NodeDeclaration> declarations;
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
};

class NodeBlockItem : public Node {
public:
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
    }
    
protected:
    int i;
    
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
        TextPosition start_pos = peek(offset).pos;
        
        for (int j = -(i >= 3 ? 3 : i); j <= 3; ++j) {
            if (!j)
                std::cout << "(here) ";
            std::cout << peek(j).text << " ";
        }
        
        std::cout << std::endl;
        
        throw ParserError(message, start_pos);
    }
    
#pragma mark - Helpers
    
    bool read_punctuator(TokenPunctuator punctuator) {
        return shift(peek().punctuator == punctuator);
    }
    
    bool read_keyword(TokenKeyword keyword) {
        return shift(peek().keyword == keyword);
    }
    
    bool read_constant() {
        return shift(peek().type == TokenType::CONSTANT);
    }
    
    bool read_string_literal() {
        return shift(peek().type == TokenType::STRING_LITERAL);
    }
    
    bool read_type_name(NodeTypeNamed &node) {
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
        read_pointer(node);
        NON_EMPTY(read_direct_declarator(node), "direct declarator expected");
        return true;
    }
    
    bool read_declaration_specifiers(std::vector<std::shared_ptr<NodeTypeSpecifier>> &node) {
        NON_EMPTY_RET(read_list(&Parser::read_type_specifier, node));
        return true;
    }
    
    bool read_direct_declarator_prefix(NodeDeclarator &node) {
        // case 1
        if (read_identifier(node.name))
            return true;
        
        // case 2: function pointer
        if (read_punctuator(TokenPunctuator::RB_OPEN)) {
            NodeDeclarator declarator;
            read_declarator(declarator);
            node.name = declarator.name;
            
            // @todo
            
            NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), "closing bracket expected");
            return true;
        }
        
        return false;
    }
    
    bool read_direct_declarator(NodeDeclarator &node) {
        NON_EMPTY(read_direct_declarator_prefix(node), "direct declarator expected");
        
        int last_good_i = i;
        
        while (!eof()) {
            // try reading parameter-list / identifier-list
            
            std::vector<NodeParameterDeclaration> parameter_list;
            std::vector<const char *> identifier_list;
            
            if (!read_punctuator(TokenPunctuator::RB_OPEN))
                break;
            
            if (read_parameter_list(parameter_list)) {
            } else if (read_identifier_list(identifier_list)) {
            }
            
            if (!read_punctuator(TokenPunctuator::RB_CLOSE))
                break;
            
            last_good_i = i;
        }
        
        i = last_good_i;
        return true;
    }
    
    bool read_identifier(const char *&text) {
        if (peek().type == TokenType::IDENTIFIER) {
            text = peek().text;
            return shift();
        }
        
        return false;
    }
    
    bool read_identifier_list(std::vector<const char *> &node) {
        return read_separated_list(&Parser::read_identifier, TokenPunctuator::COMMA, node);
    }
    
    bool read_parameter_list(std::vector<NodeParameterDeclaration> &node) {
        NON_EMPTY(read_separated_list(&Parser::read_parameter_declaration, TokenPunctuator::COMMA, node),
                  "parameter list expected");
        return true;
    }
    
    bool read_parameter_declaration(NodeParameterDeclaration &node) {
        NON_EMPTY(read_declaration_specifiers(node.specifiers), "declaration specifiers expected");
        NON_EMPTY(read_declarator(node.declarator), "declarator expected");
        return true;
    }
    
    bool read_pointer_single(NodePointer &node) {
        NON_EMPTY_RET(read_punctuator(TokenPunctuator::ASTERISK));
        read_type_qualifier_list(node.specifiers);
        
        return true;
    }
    
    bool read_pointer(NodeDeclarator &node) {
        return read_list(&Parser::read_pointer_single, node.pointers);
    }
    
    bool read_type_qualifier_list(std::vector<std::shared_ptr<NodeTypeSpecifier>> &node) {
        return read_list(&Parser::read_type_specifier, node);
    }
    
    bool read_specifier_qualifier_list(std::vector<std::shared_ptr<NodeTypeSpecifier>> &node) {
        return read_list(&Parser::read_type_specifier, node);
    }
    
#pragma mark - Structs
    
    bool read_struct_declarator(NodeDeclarator &node) {
        return read_declarator(node);
    }
    
    bool read_struct_declarator_list(std::vector<NodeDeclarator> &node) {
        return read_separated_list(&Parser::read_struct_declarator, TokenPunctuator::COMMA, node);
    }
    
    bool read_struct_declaration(NodeDeclaration &node) {
        NON_EMPTY_RET(read_specifier_qualifier_list(node.specifiers));
        read_struct_declarator_list(node.declarators);
        NON_EMPTY(read_punctuator(TokenPunctuator::SEMICOLON), "semicolon expected after struct/union member");
        return true;
    }
    
    bool read_struct_declaration_list(std::vector<NodeDeclaration> &node) {
        return read_list(&Parser::read_struct_declaration, node);
    }
    
    bool read_struct_body(NodeTypeComposed &node) {
        NON_EMPTY_RET(read_punctuator(TokenPunctuator::CB_OPEN));
        
        NON_EMPTY(read_struct_declaration_list(node.declarations), "declaration list expected");
        NON_EMPTY(read_punctuator(TokenPunctuator::CB_CLOSE), "missing closing bracket");
        
        return true;
    }
    
    bool read_type_specifier(std::shared_ptr<NodeTypeSpecifier> &node) {
        NodeTypeNamed type_name;
        if (read_type_name(type_name)) {
            node = std::make_shared<NodeTypeNamed>(type_name);
            return true;
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
                
                return true;
            }
                
            default:
                return false;
        }
        
        return false;
    }
    
    bool read_initializer(NodeDeclarator &node) {
        int assignment_expr;
        
        if (read_punctuator(TokenPunctuator::CB_OPEN)) {
            std::vector<NodeDeclarator> initializer_list;
            NON_EMPTY(read_initializer_list(initializer_list), "initializer list expected");
            read_punctuator(TokenPunctuator::COMMA);
            NON_EMPTY(read_punctuator(TokenPunctuator::CB_CLOSE), "} expected");
        } else if (read_assignment_expression(assignment_expr)) {
        } else
            return false;
        
        return true;
    }
    
    bool read_designation_initializer_pair(NodeDeclarator &node) {
        if (read_designation()) {
            NON_EMPTY(read_initializer(node), "initializer expected after designation");
        } else
            return read_initializer(node);
        
        return true;
    }
    
    bool read_initializer_list(std::vector<NodeDeclarator> &node) {
        return read_separated_list(&Parser::read_designation_initializer_pair, TokenPunctuator::COMMA, node);
    }
    
    bool read_designation() {
        NON_EMPTY_RET(read_designator_list());
        NON_EMPTY(read_punctuator(TokenPunctuator::ASSIGN), "= expected");
        return true;
    }
    
    bool read_designator_list() {
        std::vector<int> node;
        return read_list(&Parser::read_designator, node);
    }
    
    bool read_designator(int &node) {
        if (read_punctuator(TokenPunctuator::SB_OPEN)) {
            error("constant expression not implemented");
        } else if (read_punctuator(TokenPunctuator::PERIOD)) {
            const char *id;
            NON_EMPTY(read_identifier(id), "identifier expected");
        } else
            return false;
        
        return true;
    }
    
    bool read_init_declarator(NodeDeclarator &node) {
        NON_EMPTY_RET(read_declarator(node));
        
        if (read_punctuator(TokenPunctuator::ASSIGN)) {
            // also read initializer
            NON_EMPTY(read_initializer(node), "initializer expected");
        }
        
        return true;
    }
    
    bool read_init_declarator_list(std::vector<NodeDeclarator> &node) {
        return read_separated_list(&Parser::read_init_declarator, TokenPunctuator::COMMA, node);
    }
    
    bool read_declaration(NodeDeclaration &node) {
        NON_EMPTY_RET(read_declaration_specifiers(node.specifiers));
        read_init_declarator_list(node.declarators);
        NON_EMPTY(read_punctuator(TokenPunctuator::SEMICOLON), "semicolon expected");
        
        return true;
    }
    
    bool read_declaration_list(std::vector<NodeDeclaration> &node) {
        return read_list(&Parser::read_declaration, node);
    }
    
    bool read_external_declaration(std::shared_ptr<NodeExternalDeclaration> &node) {
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
            
            return true;
        } else {
            // function definition: ... declaration-list(opt) compound-statement
            
            auto n = new NodeExternalDeclarationFunction;
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            node->declarators.push_back(std::move(declarator));
            
            read_declaration_list(n->declarations);
            NON_EMPTY(read_compound_statement(), "compound statement expected");
            
            return true;
        }
        
        return false;
    }
    
#pragma mark - Statements
    
    bool read_statement() {
        if (read_labeled_statement()) {
        } else if (read_compound_statement()) {
        } else if (read_expression_statement()) {
        } else if (read_selection_statement()) {
        } else if (read_iteration_statement()) {
        } else if (read_jump_statement()) {
        } else
            return false;
        
        return true;
    }
    
    bool read_labeled_statement() {
        if (read_keyword(TokenKeyword::CASE)) {
        } else if (read_keyword(TokenKeyword::DEFAULT)) {
        } else if (peek(0).type == TokenType::IDENTIFIER && peek(1).punctuator == TokenPunctuator::COLON) {
        } else
            return false;
        
        read_statement();
        return true;
    }
    
    bool read_block_item(NodeBlockItem &node) {
        NodeDeclaration declaration;
        
        if (read_declaration(declaration)) {
        } else if (read_statement()) {
        } else
            return false;
        
        return true;
    }
    
    bool read_block_item_list(std::vector<NodeBlockItem> &node) {
        return read_list(&Parser::read_block_item, node);
    }
    
    bool read_compound_statement() {
        std::vector<NodeBlockItem> block_items;
        
        NON_EMPTY_RET(read_punctuator(TokenPunctuator::CB_OPEN));
        read_block_item_list(block_items);
        NON_EMPTY(read_punctuator(TokenPunctuator::CB_CLOSE), "closing bracket after compound statement expected");
        
        return true;
    }
    
    bool read_expression_statement() {
        int last_good_i = i;
        
        read_expression();
        
        if (!read_punctuator(TokenPunctuator::SEMICOLON)) {
            i = last_good_i;
            return false;
        }
        
        return true;
    }
    
    bool read_selection_statement() {
        NON_EMPTY_RET(read_keyword(TokenKeyword::IF));
        NON_EMPTY(read_punctuator(TokenPunctuator::RB_OPEN), "opening bracket expected");
        NON_EMPTY(read_expression(), "expression expected");
        NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), "closing bracket expected");
        NON_EMPTY(read_statement(), "statement expected");
        
        if (read_keyword(TokenKeyword::ELSE))
            NON_EMPTY(read_statement(), "statement expected");
        
        return true;
    }
    
    bool read_iteration_statement() {
        NON_EMPTY_RET(read_keyword(TokenKeyword::WHILE));
        NON_EMPTY(read_punctuator(TokenPunctuator::RB_OPEN), "opening bracket expected");
        NON_EMPTY(read_expression(), "expression expected");
        NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), "closing bracket expected");
        NON_EMPTY(read_statement(), "statement expected");
        
        return true;
    }
    
    bool read_jump_statement() {
        if (read_keyword(TokenKeyword::GOTO)) {
            const char *target;
            NON_EMPTY(read_identifier(target), "identifier expected");
        } else if (read_keyword(TokenKeyword::CONTINUE)) {
        } else if (read_keyword(TokenKeyword::BREAK)) {
        } else if (read_keyword(TokenKeyword::RETURN)) {
            read_expression();
        } else
            return false;
        
        NON_EMPTY(read_punctuator(TokenPunctuator::SEMICOLON), "semicolon expected");
        
        return true;
    }
    
#pragma mark - Expressions
    
    bool read_expression() {
        // @todo
        return false;
    }
    
    bool read_primary_expression() {
        const char *id;
        
        if (read_identifier(id)) {
        } else if (read_constant()) {
        } else if (read_string_literal()) {
        } else if (read_punctuator(TokenPunctuator::RB_OPEN)) {
            NON_EMPTY(read_expression(), "expression expected");
            NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), "closing bracket expected");
        } else
            return false;
        
        return true;
    }
    
    bool read_argument_expression_list() {
        std::vector<int> node;
        return read_separated_list(&Parser::read_assignment_expression, TokenPunctuator::COMMA, node);
    }
    
    bool read_postfix_expression() {
        if (read_primary_expression()) {
        } else if (read_punctuator(TokenPunctuator::RB_OPEN)) {
            NodeTypeNamed type_name;
            std::vector<NodeDeclarator> initializers;
            
            NON_EMPTY(read_type_name(type_name), "type-name expected");
            NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), ") expected");
            
            NON_EMPTY(read_punctuator(TokenPunctuator::CB_OPEN), "{ expected");
            NON_EMPTY(read_initializer_list(initializers), "initializer list expected");
            read_punctuator(TokenPunctuator::COMMA);
            NON_EMPTY(read_punctuator(TokenPunctuator::CB_CLOSE), "} expected");
        } else
            return false;
        
        while (!eof()) {
            if (read_punctuator(TokenPunctuator::SB_OPEN)) {
                NON_EMPTY(read_expression(), "expression expected");
                NON_EMPTY(read_punctuator(TokenPunctuator::SB_CLOSE), "] expected");
            } else if (read_punctuator(TokenPunctuator::RB_OPEN)) {
                read_argument_expression_list();
                NON_EMPTY(read_punctuator(TokenPunctuator::RB_CLOSE), ") expected");
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
        
        return true;
    }
    
    bool read_unary_operator() {
        switch (peek().punctuator) {
            case TokenPunctuator::BIT_AND:
            case TokenPunctuator::ASTERISK:
            case TokenPunctuator::PLUS:
            case TokenPunctuator::MINUS:
            case TokenPunctuator::BIT_NOT:
            case TokenPunctuator::LOG_NOT:
                return shift();
                
            default:
                return false;
        }
    }
    
    bool read_unary_expression() {
        if (read_punctuator(TokenPunctuator::PLUSPLUS) || read_punctuator(TokenPunctuator::MINUSMINUS)) {
            NON_EMPTY(read_unary_expression(), "unary expression expected");
        } else if (read_unary_operator()) {
            NON_EMPTY(read_cast_expression(), "cast expression expected");
        } else if (read_postfix_expression()) {
        } else
            return false;
        
        return true;
    }
    
    bool read_cast_expression() {
        int last_good_i = i;
        
        NodeTypeNamed type_name;
        if (read_punctuator(TokenPunctuator::RB_OPEN) &&
            read_type_name(type_name) &&
            read_punctuator(TokenPunctuator::RB_CLOSE)) {
            NON_EMPTY(read_cast_expression(), "cast expression expected");
            return true;
        } else {
            // backtrack
            i = last_good_i;
        }
        
        return read_unary_expression();
    }
    
    bool read_assignment_expression(int &node) {
        return read_cast_expression(); // @todo
    }
};

#undef NON_EMPTY
#undef NON_EMPTY_RET

#endif /* Parser_hpp */

