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
        error(err, i); \
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

class NodePointer : public Node {
public:
    virtual void dump(const std::string &indent) {
        std::cout << indent << "Pointer" << std::endl;
    }
};

class NodeDeclaration : public Node {
public:
    virtual void dump(const std::string &indent) {
        std::cout << indent << "Declaration" << std::endl;
    }
};

class Parser {
public:
    Parser(Lexer lexer) : lexer(lexer) {
    }
    
    void parse() {
        NodeStack stack;
        current_stack = &stack;
        
        int i = 0;
        read_list(i, &Parser::read_external_declaration);
        
        for (auto &child : *current_stack)
            child->dump("");
        
        if (!eof(i))
            error("declaration expected", i);
    }
    
protected:
    NodeStack *current_stack;
    
    Lexer lexer;
    std::vector<Token> token_queue;
    
    bool eof(int i) {
        return (lexer.has_ended() && (int)token_queue.size() < i) || peek(i).type == TokenType::END;
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
    
    [[noreturn]] void error(const std::string &message, int offset) {
        TextPosition start_pos = peek(offset).pos;
        throw ParserError(message, start_pos);
    }
    
#pragma mark - Helpers
    
    bool read_punctuator(int &i, TokenPunctuator punctuator) {
        if (peek(i).punctuator == punctuator) {
            ++i;
            return true;
        }
        
        return false;
    }
    
    bool read_list(int &i, bool (Parser::*method)(int &)) {
        int initial_i = i;
        
        auto list = std::make_shared<NodeList>();
        
        auto previous_stack = current_stack;
        current_stack = &list->children;
        
        while (!eof(i))
            if (!(this->*method)(i))
                break;
        
        current_stack = previous_stack;
        current_stack->push_back(list);
        
        return i != initial_i;
    }
    
    bool read_separated_list(int &i, bool (Parser::*method)(int &), TokenPunctuator separator) {
        auto list = std::make_shared<NodeList>();
        
        auto previous_stack = current_stack;
        current_stack = &list->children;
        
        int initial_i = i;
        int last_good_i = i;
        
        while (!eof(i)) {
            if (!(this->*method)(i))
                break;
            
            last_good_i = i;
            
            if(!read_punctuator(i, separator))
                break;
        }
        
        current_stack = previous_stack;
        current_stack->push_back(list);
        
        i = last_good_i;
        return i != initial_i;
    }
    
#pragma mark - Productions
    
    bool read_declarator(int &i) {
        read_pointer(i);
        NON_EMPTY(read_direct_declarator(i), "direct declarator expected");
        return true;
    }
    
    bool read_declaration_specifiers(int &i) {
        NON_EMPTY_RET(read_list(i, &Parser::read_type_specifier));
        return true;
    }
    
    bool read_direct_declarator_prefix(int &i) {
        // case 1
        if (read_identifier(i))
            return true;
        
        // case 2
        if (read_punctuator(i, TokenPunctuator::RB_OPEN)) {
            read_declarator(i);
            NON_EMPTY(read_punctuator(i, TokenPunctuator::RB_CLOSE), "closing bracket expected");
            return true;
        }
        
        return false;
    }
    
    bool read_direct_declarator(int &i) {
        NON_EMPTY(read_direct_declarator_prefix(i), "direct declarator expected");
        
        int last_good_i = i;
        
        while (!eof(i)) {
            // try reading parameter-type-list / identifier-list
            
            if (!read_punctuator(i, TokenPunctuator::RB_OPEN))
                break;
            
            read_parameter_list(i) || read_identifier_list(i);
            
            if (!read_punctuator(i, TokenPunctuator::RB_CLOSE))
                break;
            
            last_good_i = i;
        }
        
        i = last_good_i;
        return true;
    }
    
    bool read_identifier(int &i) {
        if (peek(i).type == TokenType::IDENTIFIER) {
            current_stack->push_back(std::make_shared<NodeIdentifier>(peek(i).text));
            ++i;
            
            return true;
        }
        
        return false;
    }
    
    bool read_identifier_list(int &i) {
        return read_separated_list(i, &Parser::read_identifier, TokenPunctuator::COMMA);
    }
    
    bool read_parameter_list(int &i) {
        NON_EMPTY(read_separated_list(i, &Parser::read_parameter_declaration, TokenPunctuator::COMMA), "parameter list expected");
        return true;
    }
    
    bool read_parameter_declaration(int &i) {
        NON_EMPTY(read_declaration_specifiers(i), "declaration specifiers expected");
        NON_EMPTY(read_declarator(i), "declarator expected");
        return true;
    }
    
    bool read_pointer_single(int &i) {
        NON_EMPTY_RET(read_punctuator(i, TokenPunctuator::ASTERISK));
        read_type_qualifier_list(i);
        
        current_stack->push_back(std::make_shared<NodePointer>());
        
        return true;
    }
    
    bool read_pointer(int &i) {
        return read_list(i, &Parser::read_pointer_single);
    }
    
    bool read_type_qualifier_list(int &i) {
        return read_list(i, &Parser::read_type_specifier);
    }
    
    bool read_specifier_qualifier_list(int &i) {
        return read_list(i, &Parser::read_type_specifier);
    }
    
#pragma mark - Structs
    
    bool read_struct_declarator(int &i) {
        return read_declarator(i);
    }
    
    bool read_struct_declarator_list(int &i) {
        return read_separated_list(i, &Parser::read_struct_declarator, TokenPunctuator::COMMA);
    }
    
    bool read_struct_declaration(int &i) {
        NON_EMPTY_RET(read_specifier_qualifier_list(i));
        read_struct_declarator_list(i);
        NON_EMPTY(read_punctuator(i, TokenPunctuator::SEMICOLON), "semicolon expected after struct/union member");
        return true;
    }
    
    bool read_struct_declaration_list(int &i) {
        return read_list(i, &Parser::read_struct_declaration);
    }
    
    bool read_struct_body(int &i) {
        NON_EMPTY_RET(read_punctuator(i, TokenPunctuator::CB_OPEN));
        
        NON_EMPTY(read_struct_declaration_list(i), "declaration list expected");
        NON_EMPTY(read_punctuator(i, TokenPunctuator::CB_CLOSE), "missing closing bracket");
        
        auto node = std::make_shared<NodeStructMembers>();
        auto sdl_node = current_stack->back();
        current_stack->pop_back();
        
        node->children = dynamic_cast<NodeList *>(sdl_node.get())->children;
        current_stack->push_back(node);
        
        return true;
    }
    
    bool read_type_specifier(int &i) {
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
                ++i;
                
                return true;
                
            case TokenKeyword::STRUCT:
            case TokenKeyword::UNION: {
                ++i;
                
                bool has_identifier = read_identifier(i);
                bool has_body = peek(i).punctuator == TokenPunctuator::CB_OPEN;
                
                if (!has_body && !has_identifier)
                    error("struct/union without identifier or body", i);
                
                if (has_body)
                    NON_EMPTY(read_struct_body(i), "struct/union body expected");
            }
                
            default:
                return false;
        }
        
        return false;
    }
    
    bool read_initializer(int &i) {
        error("not yet implemented", i);
    }
    
    bool read_init_declarator(int &i) {
        NON_EMPTY_RET(read_declarator(i));
        
        if (read_punctuator(i, TokenPunctuator::ASSIGN)) {
            // also read initializer
            NON_EMPTY(read_initializer(i), "initializer expected");
        }
        
        return true;
    }
    
    bool read_init_declarator_list(int &i) {
        return read_separated_list(i, &Parser::read_init_declarator, TokenPunctuator::COMMA);
    }
    
    bool read_declaration(int &i) {
        NON_EMPTY_RET(read_declaration_specifiers(i));
        read_init_declarator_list(i);
        NON_EMPTY(read_punctuator(i, TokenPunctuator::SEMICOLON), "semicolon expected");
        
        return true;
    }
    
    bool read_declaration_list(int &i) {
        return read_list(i, &Parser::read_declaration);
    }
    
    bool read_external_declaration(int &i) {
        NON_EMPTY_RET(read_declaration_specifiers(i));
        NON_EMPTY(read_declarator(i), "declarator expected");
        
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
                read_init_declarator_list(i);
            }
            
            NON_EMPTY(read_punctuator(i, TokenPunctuator::SEMICOLON), "semicolon expected");
            
            return true;
        } else {
            // function definition: ... declaration-list(opt) compound-statement
            
            read_declaration_list(i);
            NON_EMPTY(read_compound_statement(i), "compound statement expected");
            
            return true;
        }
        
        // init-declarator-list: read_list(init-declarator, ',')
        // init-declarator: declarator
        //                | declarator '=' initializer
        
        error("not yet implemented", i);
    }
    
#pragma mark - Statements
    
    bool read_statement(int &i) {
        // @todo not yet implemented
        return false;
    }
    
    bool read_block_item(int &i) {
        return read_declaration(i) || read_statement(i);
    }
    
    bool read_block_item_list(int &i) {
        return read_list(i, &Parser::read_block_item);
    }
    
    bool read_compound_statement(int &i) {
        NON_EMPTY_RET(read_punctuator(i, TokenPunctuator::CB_OPEN));
        read_block_item_list(i);
        NON_EMPTY(read_punctuator(i, TokenPunctuator::CB_CLOSE), "bracket after compound statement expected");
        
        return true;
    }
};

#undef NON_EMPTY
#undef NON_EMPTY_RET

#endif /* Parser_hpp */
