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
#include <vector>

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

class Parser {
public:
    Parser(Lexer lexer) : lexer(lexer) {
    }
    
    void parse() {
        read_list(0, &Parser::read_external_declaration);
    }
    
protected:
    Lexer lexer;
    std::vector<Token> token_queue;
    
    bool eof(int i) {
        return lexer.has_ended() && (int)token_queue.size() < i;
    }
    
    Token &peek(int i) {
        if (i < (int)token_queue.size())
            return token_queue[i];
        
        while (!lexer.has_ended() && (int)token_queue.size() <= i) {
            printf("reading token...\n");
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
        if (peek(i).type == TokenType::IDENTIFIER)
            return i + 1;
        
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
        if (peek(i).type == TokenType::IDENTIFIER)
            return i + 1;
        
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
        while (!eof(i)) {
            int j = (this->*method)(i);
            if (i == j)
                break;
            
            i = j;
        }
        
        return i;
    }
    
    int read_separated_list(int i, int (Parser::*method)(int), TokenPunctuator separator) {
        while (!eof(i)) {
            int j = (this->*method)(i);
            if (i == j)
                break;
            
            i = j;
            
            if(peek(i).punctuator != separator)
                break;
            
            ++i;
        }
        
        return i;
    }
    
    int read_external_declaration(int i) {
        /*
         (6.9) external-declaration: function-definition
                                     declaration
         
         (6.9.1) function-definition:
                    declaration-specifiers declarator declaration-listopt compound-statement
         
         (6.9.1) declaration-list: declaration
                                    declaration-list declaration
         
         (6.7) declaration:
                    declaration-specifiers init-declarator-listopt ;
                    static_assert-declaration
         */
        
        // @todo static_assert-declaration
        
        // step 1. read declaration-specifiers
        
        NON_EMPTY(i, read_list(i, &Parser::read_type_specifier), "type list expected");
        
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
