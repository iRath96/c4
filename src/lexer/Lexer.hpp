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

class Lexer {
public:
    Lexer(std::istream *input) : input(input) {
    }
    
protected:
    std::istream *input;
    int index;
    
    TextPosition pos = { .line = 1, .column = 1 };
    
    std::string consume(int length) {
        input->seekg(index, input->beg);
        
        char buffer[length];
        
        input->read(buffer, length);
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
        input->clear(); // clear state flaags
        input->seekg(index + offset, input->beg);
        return input->peek();
    }
    
    bool eof() {
        return input->eof();
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
