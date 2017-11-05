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

#define NON_EMPTY(i, stmt, err) {\
    int j = stmt; \
    if (i == j) \
        error(err, i); \
    i = j; \
}

#define NON_EMPTY_RET(i, stmt) {\
    int j = stmt; \
    if (i == j) \
        return i; \
    i = j; \
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
    virtual void dump(const std::string &indent) = 0;
};

typedef std::vector<std::shared_ptr<Node>> NodeStack;

class NodeList : public Node {
public:
    NodeStack children;
    
    virtual void dump(const std::string &indent) {
        std::cout << indent << "List" << std::endl;
        for (auto &child : children)
            child->dump(indent + "  ");
    }
};

class NodeStructMembers : public NodeList {
public:
    virtual void dump(const std::string &indent) {
        std::cout << indent << "Struct" << std::endl;
        for (auto &child : children)
            child->dump(indent + "  ");
    }
};

class NodeIdentifier : public Node {
public:
    const char *id;
    NodeIdentifier(const char *id) : id(id) {}
    
    virtual void dump(const std::string &indent) {
        std::cout << indent << "Identifier " << id << std::endl;
    }
};

class NodeTypeSpecifier : public Node {
public:
    const char *id;
    NodeTypeSpecifier(const char *id) : id(id) {}
    
    virtual void dump(const std::string &indent) {
        std::cout << indent << "TypeSpecifier " << id << std::endl;
    }
};

class Parser {
public:
    Parser(Lexer lexer) : lexer(lexer) {
    }
    
    void parse() {
        NodeStack stack;
        current_stack = &stack;
        
        read_list(0, &Parser::read_external_declaration);
    }
    
protected:
    NodeStack *current_stack;
    
    Lexer lexer;
    std::vector<Token> token_queue;
    
    bool eof(int i) {
        return lexer.has_ended() && (int)token_queue.size() < i;
    }
    
    Token &peek(int i) {
        if (i < (int)token_queue.size())
            return token_queue[i];
        
        while (!lexer.has_ended() && (int)token_queue.size() <= i) {
            // printf("reading token...\n");
            token_queue.push_back(lexer.next_token());
        }
        
        return token_queue[token_queue.size() - 1]; // size() <= i+1
    }
    
    /* 6.7.6 */ int read_declarator(int i) {
        i = read_pointer(i);
        NON_EMPTY(i, read_direct_declarator(i), "direct declarator expected");
        return i;
    }
    
    /* 6.7.6 */ int read_declaration_specifiers(int i) {
        NON_EMPTY(i, read_list(i, &Parser::read_type_specifier), "type list expected");
        return i;
    }
    
    int read_direct_declarator_prefix(int i) {
        // case 1
        int j = read_identifier(i);
        if (i != j)
            return j;
        
        // case 2
        if (peek(i).punctuator == TokenPunctuator::RB_OPEN) {
            i = read_declarator(i + 1);
            
            if (peek(i).punctuator != TokenPunctuator::RB_CLOSE)
                error("closing bracket expected", i);
            
            return i + 1;
        }
        
        return i;
    }
    
    /* 6.7.6 */ int read_direct_declarator(int i) {
        NON_EMPTY(i, read_direct_declarator_prefix(i), "direct declarator expected");
        
        int last_good_i = i;
        
        while (!eof(i)) {
            // try reading parameter-type-list / identifier-list
            
            if (peek(i).punctuator != TokenPunctuator::RB_OPEN)
                break;
            
            int j = read_parameter_list(i);
            if (i == j)
                j = read_identifier_list(i);
            
            if (peek(i).punctuator == TokenPunctuator::RB_CLOSE)
                last_good_i = ++i;
        }
        
        return last_good_i;
    }
    
    int read_identifier(int i) {
        if (peek(i).type == TokenType::IDENTIFIER) {
            current_stack->push_back(std::make_shared<NodeIdentifier>(peek(i).text));
            return i + 1;
        }
        
        return i;
    }
    
    /* 6.7.6 */ int read_identifier_list(int i) {
        return read_separated_list(i, &Parser::read_identifier, TokenPunctuator::COMMA);
    }
    
    /* 6.7.6 */ int read_parameter_list(int i) {
        NON_EMPTY(i, read_separated_list(i, &Parser::read_parameter_declaration, TokenPunctuator::COMMA), "parameter list expected");
        return i;
    }
    
    /* 6.7.6 */ int read_parameter_declaration(int i) {
        NON_EMPTY(i, read_declaration_specifiers(i), "declaration specifiers expected");
        NON_EMPTY(i, read_declarator(i), "declarator expected");
        return i;
    }
    
    int read_pointer_single(int i) {
        if (peek(i).punctuator != TokenPunctuator::ASTERISK)
            return i;
        
        i = read_type_qualifier_list(i);
        
        return i;
    }
    
    /* 6.7.6 */ int read_pointer(int i) {
        return read_list(i, &Parser::read_pointer_single);
    }
    
    int read_type_qualifier_list(int i) {
        return read_list(i, &Parser::read_type_specifier);
    }
    
    int read_specifier_qualifier_list(int i) {
        return read_list(i, &Parser::read_type_specifier);
    }
    
#pragma mark - Structs
    
    /* 6.7.2.1 */ int read_struct_declarator(int i) {
        return read_declarator(i);
    }
    
    /* 6.7.2.1 */ int read_struct_declarator_list(int i) {
        return read_separated_list(i, &Parser::read_struct_declarator, TokenPunctuator::COMMA);
    }
    
    /* 6.7.2.1 */ int read_struct_declaration(int i) {
        NON_EMPTY_RET(i, read_specifier_qualifier_list(i));
        
        i = read_struct_declarator_list(i);
        if (peek(i).punctuator != TokenPunctuator::SEMICOLON)
            error("semicolon expected after struct/union member", i);
        
        return i + 1;
    }
    
    /* 6.7.6 */ int read_struct_declaration_list(int i) {
        return read_list(i, &Parser::read_struct_declaration);
    }
    
    int read_struct_body(int i) {
        if (peek(i).punctuator != TokenPunctuator::CB_OPEN)
            return i;
        
        ++i;
        
        NON_EMPTY(i, read_struct_declaration_list(i), "declaration list expected");
        
        if (peek(i).punctuator != TokenPunctuator::CB_CLOSE)
            error("missing closing bracket", i);
        
        auto node = std::make_shared<NodeStructMembers>();
        auto sdl_node = current_stack->back();
        current_stack->pop_back();
        
        node->children = dynamic_cast<NodeList *>(sdl_node.get())->children;
        current_stack->push_back(node);
        
        return i + 1;
    }
    
    int read_type_specifier(int i) {
        Token &token = peek(i);
        switch (token.keyword) {
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
                current_stack->push_back(std::make_shared<NodeTypeSpecifier>(token.text));
                return i + 1;
                
            case TokenKeyword::STRUCT:
            case TokenKeyword::UNION: {
                bool has_identifier = peek(++i).type == TokenType::IDENTIFIER;
                i += has_identifier ? 1 : 0;
                
                bool has_body = peek(i).punctuator == TokenPunctuator::CB_OPEN;
                if (!has_body && !has_identifier)
                    error("struct/union without identifier or body", i);
                
                if (has_body)
                    NON_EMPTY(i, read_struct_body(i), "struct/union body expected");
            }
                
            default:
                return i;
        }
    }
    
    int read_list(int i, int (Parser::*method)(int)) {
        auto list = std::make_shared<NodeList>();
        
        auto previous_stack = current_stack;
        current_stack = &list->children;
        
        while (!eof(i)) {
            int j = (this->*method)(i);
            if (i == j)
                break;
            
            i = j;
        }
        
        current_stack = previous_stack;
        current_stack->push_back(list);
        
        return i;
    }
    
    int read_separated_list(int i, int (Parser::*method)(int), TokenPunctuator separator) {
        auto list = std::make_shared<NodeList>();
        
        auto previous_stack = current_stack;
        current_stack = &list->children;
        
        while (!eof(i)) {
            int j = (this->*method)(i);
            if (i == j)
                break;
            
            i = j;
            
            if(peek(i).punctuator != separator)
                break;
            
            ++i;
        }
        
        current_stack = previous_stack;
        current_stack->push_back(list);
        
        return i;
    }
    
    int read_initializer(int i) {
        error("not yet implemented", i);
    }
    
    int read_init_declarator(int i) {
        NON_EMPTY_RET(i, read_declarator(i));
        
        if (peek(i).punctuator == TokenPunctuator::ASSIGN) {
            // also read initializer
            NON_EMPTY(i, read_initializer(i), "initializer expected");
        }
        
        return i;
    }
    
    int read_init_declarator_list(int i) {
        return read_separated_list(i, &Parser::read_init_declarator, TokenPunctuator::COMMA);
    }
    
    int read_external_declaration(int i) {
        NON_EMPTY(i, read_declaration_specifiers(i), "type list expected");
        NON_EMPTY(i, read_declarator(i), "declarator expected");
        
        bool needs_declaration_list = peek(i).punctuator == TokenPunctuator::COMMA;
        bool needs_initialization = peek(i).punctuator == TokenPunctuator::ASSIGN;
        bool is_declaration = needs_initialization || needs_declaration_list || peek(i).punctuator == TokenPunctuator::SEMICOLON;
        
        if (is_declaration) {
            // declaration: ... (',' init-declarator-list(opt))(opt) ;
            
            if (needs_initialization) {
                ++i; // consume assign
                read_initializer(i);
                
                needs_initialization = peek(i).punctuator == TokenPunctuator::COMMA;
            }
            
            if (needs_declaration_list) {
                ++i; // jump over comma
                i = read_init_declarator_list(i);
            }
            
            if (peek(i).punctuator != TokenPunctuator::SEMICOLON)
                error("semicolon expected", i);
            
            std::cout << "read a declaration" << std::endl;
            
            return i;
        } else {
            // function definition: ... declaration-list(opt) compound-statement
            error("function-definition not yet implemented", i);
        }
        
        // init-declarator-list: read_list(init-declarator, ',')
        // init-declarator: declarator
        //                | declarator '=' initializer
        
        for (auto &child : *current_stack)
            child->dump("");
        
        error("not yet implemented", i);
    }
    
    [[noreturn]] void error(const std::string &message, int offset) {
        TextPosition start_pos = peek(offset).pos;
        throw ParserError(message, start_pos);
    }
};

#undef NON_EMPTY
#undef NON_EMPTY_RET

#endif /* Parser_hpp */
