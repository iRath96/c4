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
    int line;
    int column;
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

class Lexer {
public:
    Lexer(LexerInput input) : input(input) {
    }
    
protected:
    LexerInput input;
    
    int index;
    
    TextPosition pos = { .line = 1, .column = 1 };
    
    std::string consume(int length) {
        const char *buffer = input.data + index;
        index += length;
        
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
        
        return input.data[index + offset];
    }
    
    bool eof(int offset) {
        return index + offset >= input.length;
    }
    
public:
    Token next_token();
    
protected:
    Token create_token(TokenType type, int length);
    
    int read_whitespace();
    int read_comment();
    
    int read_punctuator();
    int read_identifier();
    int read_string();
    int read_constant();
};

#endif /* Lexer_hpp */
