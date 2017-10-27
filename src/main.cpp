//
//  main.cpp
//  c4
//
//  Created by Alexander Rath on 26.10.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include <iostream>
#include <fstream>
#include "lexer/Lexer.hpp"

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
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cout << "Could not open " << filename << " for reading." << std::endl;
        exit(1);
    }
    
    Lexer lexer(&file);
    while (true) {
        Token t = lexer.next_token();
        
        if (t.type == TokenType::END)
            return;
        
        std::cout << filename << ":"
            << t.pos.line << ":" << t.pos.column << ": "
            << token_type_name(t.type) << " "
            << t.text
            << std::endl;
    }
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
