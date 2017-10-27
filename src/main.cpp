//
//  main.cpp
//  c4
//
//  Created by Alexander Rath on 26.10.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include <iostream>
#include <fstream>
#include "Lexer.h"

std::string token_type_name(TokenType type) {
    switch (type) {
        case TokenType::KEYWORD: return "keyword";
        case TokenType::IDENTIFIER: return "identifier";
        case TokenType::CONSTANT: return "constant";
        case TokenType::STRING_LITERAL: return "string-literal";
        case TokenType::PUNCTUATOR: return "punctuator";
        case TokenType::END: return "end";
    }
}

void tokenize(std::string filename) {
    std::ifstream file(filename, std::ios::binary | std::ios::ate);
    if (!file.is_open()) {
        std::cout << "Could not open " << filename << " for reading." << std::endl;
        exit(1);
    }
    
    LexerInput input;
    input.length = (int)file.tellg();
    
    char *buffer = (char *)malloc(input.length);
    file.seekg(0, file.beg);
    file.read(buffer, input.length);
    
    input.data = buffer;
    
    Lexer lexer(input);
    while (true) {
        try {
            Token t = lexer.next_token();
            
            if (t.type == TokenType::END)
                break;
            
            std::cout << filename << ":"
                << t.pos.line << ":" << t.pos.column << ": "
                << token_type_name(t.type) << " "
                << t.text
                << std::endl;
        } catch (LexerError e) {
            std::cout << filename << ":"
                << e.pos.line << ":" << e.pos.column << ": "
                << "error: "
                << e.message
                << std::endl;
            
            exit(1);
        }
    }
    
    free(buffer);
}

int main(int argc, const char *argv[]) {
    for (int i = 1; i < argc; ++i) {
        if (!strcmp(argv[i], "--tokenize"))
            tokenize(argv[++i]);
        else {
            printf("Unrecognized argument: %s\n", argv[i]);
            exit(1);
        }
    }
}
