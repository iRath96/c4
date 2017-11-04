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

class ParserError {
public:
    std::string message;
    ParserError(const std::string &message)
    : message(message) {
        
    }
};

class Parser {
public:
    Parser(Lexer lexer) : lexer(lexer) {
    }
    
    void parse() {
        while (!lexer.has_ended()) {
            parse_external_declaration();
        }
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
    
    int read_type_specifiers(int i) {
        while (!eof(i)) {
            Token &token = peek(i);
            if (token.type != TokenType::KEYWORD)
                return i;
            
            switch (token.meta.keyword) {
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
                    ++i;
                    break;
                    
                case TokenKeyword::STRUCT:
                case TokenKeyword::UNION: {
                    ++i;
                    
                    bool has_identifier = peek(i).type == TokenType::IDENTIFIER;
                    i += has_identifier ? 1 : 0;
                    
                    bool has_body = peek(i).type == TokenType::PUNCTUATOR && peek(i).meta.punctuator == TokenPunctuator::CB_OPEN;
                    if (!has_body && !has_identifier)
                        error("struct/union without identifier or body", i);
                    
                    if (has_body)
                        error("struct/union body reading not yet implemented", i);
                    
                    break;
                }
                    
                default:
                    // unrecognized token
                    return i;
            }
        }
        
        return i;
    }
    
    void parse_external_declaration() {
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
        int i = read_type_specifiers(0);
        printf("%d\n", i);
        if (i == 0)
            error("type list expected", 0);
        
        error("not yet implemented", i);
    }
    
    [[noreturn]] void error(const std::string &message, int offset) {
        //TextPosition start_pos = pos;
        //consume(offset);
        throw ParserError(message);
    }
};

#endif /* Parser_hpp */
