//
//  Lexer.hpp
//  c4
//
//  Created by Alexander Rath on 26.10.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#ifndef Lexer_hpp
#define Lexer_hpp

#include <iostream>
#include <sstream>
#include <map>

struct TextPosition {
    int index, length;
    int line, column;
};

enum TokenType {
    KEYWORD, IDENTIFIER, CONSTANT, STRING_LITERAL, PUNCTUATOR,
    END
};

struct Token {
    TextPosition pos;
    
    TokenType type;
    std::string text;
};

struct LexerInput {
    const char *data;
    int length;
};

class LexerError {
public:
    TextPosition pos;
    std::string message;
    
    LexerError(const std::string &message, TextPosition pos, int length) {
        this->message = message;
        this->pos = pos;
        this->pos.length = length;
    }
};

class Lexer {
public:
    Lexer(LexerInput input) : input(input) {
    }
    
protected:
    LexerInput input;
    
    TextPosition pos = { .index = 0, .line = 1, .column = 1, .length = 0 };
    
    std::string consume(int length) {
        const char *buffer = input.data + pos.index;
        pos.index += length;
        
        for (int i = 0; i < length; ++i) {
            // update position
            if (buffer[i] == '\n') {
                ++pos.line;
                pos.column = 1;
            } else {
                ++pos.column;
            }
        }
        
        return std::string(buffer, length);
    }
    
    char peek(int offset) {
        if (eof(offset))
            return 0;
        
        return input.data[pos.index + offset];
    }
    
    bool eof(int offset) {
        return pos.index + offset >= input.length;
    }
    
public:
    Token next_token();
    
protected:
    Token create_token(TokenType type, int length);
    void error(const std::string &message, int offset);
    
    int read_whitespace();
    int read_comment();
    
    int read_punctuator();
    int read_identifier();
    int read_string();
    int read_char();
    int read_constant();
};

#endif /* Lexer_hpp */
