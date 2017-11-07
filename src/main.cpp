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
#include "Parser.h"

const char *token_type_name(TokenType type) {
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

Lexer create_lexer(const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        std::cout << "Could not open " << filename << " for reading." << std::endl;
        exit(1);
    }
    
    fseek(f, 0, SEEK_END);
    
    int length = (int)ftell(f);
    
    char *buffer = (char *)malloc(length);
    fseek(f, 0, SEEK_SET);
    fread(buffer, length, 1, f);
    fclose(f);
    
    return Lexer(buffer, length);
}

void tokenize(const char *filename) {
    Lexer lexer = create_lexer(filename);
    try {
        while (true) {
            Token t = lexer.next_token();
        
            if (t.type == TokenType::END)
                break;
        
            printf("%s:%d:%d: %s %s\n", filename, t.pos.line, t.pos.column, token_type_name(t.type), t.text);
        }
    } catch (LexerError e) {
        fprintf(stderr, "%s:%d:%d: error: %s\n", filename, e.end_pos.line, e.end_pos.column, e.message.c_str());
        exit(1);
    }
}

bool print_ast = false;

void parse(const char *filename) {
    Parser parser(create_lexer(filename));
    try {
        parser.parse();
    } catch (LexerError e) {
        fprintf(stderr, "%s:%d:%d: error: %s\n", filename, e.end_pos.line, e.end_pos.column, e.message.c_str());
        exit(1);
    } catch (ParserError e) {
        parser.print_debug_tree();
        parser.print_context();
        
        fprintf(stderr, "%s:%d:%d: parser error: %s\n", filename, e.pos.line, e.pos.column, e.message.c_str());
        exit(1);
    }
    
    if (print_ast) {
        for (auto &decl : parser.declarations) {
            decl->describe(std::cout, "");
        }
    }
}

int main(int argc, const char *argv[]) {
    for (int i = 1; i < argc; ++i) {
        if (!strcmp(argv[i], "--tokenize"))
            tokenize(argv[++i]);
        else if (!strcmp(argv[i], "--ast"))
            print_ast = true;
        else if (!strcmp(argv[i], "--parse"))
            parse(argv[++i]);
        else {
            printf("Unrecognized argument: %s\n", argv[i]);
            exit(1);
        }
    }
}
