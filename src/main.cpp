//
//  main.cpp
//  c4
//
//  Created by Alexander Rath on 26.10.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include <iostream>
#include <fstream>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
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
    return "unknown";
}

void tokenize(std::string filename) {
    FILE *f = fopen(filename.c_str(), "rb");
    if (!f) {
        std::cout << "Could not open " << filename << " for reading." << std::endl;
        exit(1);
    }
    
    fseek(f, 0, SEEK_END);
    
    LexerInput input;
    input.length = (int)ftell(f);
    
    char *buffer = (char *)malloc(input.length);
    fseek(f, 0, SEEK_SET);
    fread(buffer, input.length, 1, f);
    fclose(f);
    
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
