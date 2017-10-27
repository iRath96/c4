//
//  Lexer.cpp
//  c4
//
//  Created by Alexander Rath on 26.10.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include "Lexer.h"

bool is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n';
}

bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

bool is_nondigit(char c) {
    return is_alpha(c) || c == '_';
}

bool is_num(char c) {
    return c >= '0' && c <= '9';
}

bool is_alphanum(char c) {
    return is_alpha(c) || is_num(c);
}

Token Lexer::next_token() {
    peek(0); // make sure eof flag is set appropriately
    
    while (!eof(0)) {
        int length = 0;
        if ((length = read_whitespace()) || (length = read_comment())) {
            // skip whitespace and comments
            consume(length);
            continue;
        }
        else if ((length = read_punctuator()))
            return create_token(TokenType::PUNCTUATOR, length);
        else if ((length = read_string()))
            return create_token(TokenType::STRING_LITERAL, length);
        else if ((length = read_identifier()))
            return create_token(TokenType::IDENTIFIER, length);
        else if ((length = read_constant()))
            return create_token(TokenType::CONSTANT, length);
        
        // no token was found
        exit(1);
    }
    
    return create_token(TokenType::END, 0);
}

Token Lexer::create_token(TokenType type, int length) {
    Token token;
    
    token.pos = pos;
    token.type = type;
    token.text = consume(length);
    
    if (token.type == TokenType::IDENTIFIER) {
#define K(x) if (token.text == x) token.type = TokenType::KEYWORD; else
        K("auto")       K("if")         K("unsigned")
        K("break")      K("inline")     K("void")
        K("case")       K("int")        K("volatile")
        K("char")       K("long")       K("while")
        K("const")      K("register")   K("_Alignas")
        K("continue")   K("restrict")   K("_Alignof")
        K("default")    K("return")     K("_Atomic")
        K("do")         K("short")      K("_Bool")
        K("double")     K("signed")     K("_Complex")
        K("else")       K("sizeof")     K("_Generic")
        K("enum")       K("static")     K("_Imaginary")
        K("extern")     K("struct")     K("_Noreturn")
        K("float")      K("switch")     K("_Static_assert")
        K("for")        K("typedef")    K("_Thread_local")
        K("goto")       K("union")
        ;
#undef K
    }
    
    return token;
}

int Lexer::read_whitespace() {
    int i = 0;
    while (is_whitespace(peek(i)))
        ++i;
    return i;
}

int Lexer::read_string() {
    if (peek(0) != '"')
        // not a string
        return 0;
    
    int i = 1;
    while (!eof(i)) {
        char c = peek(i);
        if (c == '"')
            return i + 1;
        i += c == '\\' ? 2 : 1;
    }
    
    printf("EOF encountered while reading string\n");
    return 0;
}

int Lexer::read_comment() {
    if (peek(0) != '/' || peek(1) != '*')
        // not a comment
        return 0;
    
    int i = 2;
    while (!eof(i + 1)) {
        if (peek(i) == '*' && peek(i + 1) == '/')
            return i + 2;
        
        ++i;
    }
    
    printf("EOF encountered while reading comment\n");
    return 0;
}

int Lexer::read_punctuator() {
    char c = peek(0), d = peek(1);
    
    if (c == '%' && d == ':' && peek(2) == '%' && peek(3) == ':')
        return 4;
    
    if (c == '.' && d == '.' && peek(2) == '.')
        return 3;
    
    if (c == '-' && d == '>') return 2;
    if ((c == '+' || c == '-' || c == '&' || c == '|' || c == '<' || c == '>' || c == '#')
        && d == c)
        return 2;
    
    if ((c == '*' || c == '/' || c == '%' || c == '+' || c == '-' || c == '&' || c == '^' || c == '|')
        && d == '=')
        return 2;
    
    if (c == '<' && (d == ':' || d == '%')) return 2;
    if ((c == ':' || c == '%') && d == '>') return 2;
    if (c == '%' && d == ':') return 2;
    
    if (c == '[' || c == ']' || c == '(' || c == ')' || c == '{' || c == '}') return 1;
    if (c == '#' || c == ',' || c == '=' || c == '?' || c == ':' || c == ';') return 1;
    if (c == '-' || c == '+' || c == '*' || c == '/' || c == '%') return 1;
    if (c == '~' || c == '&' || c == '|' || c == '^') return 1;
    if (c == ',') return 1;
    
    return 0;
}

int Lexer::read_identifier() {
    if (!is_nondigit(peek(0)))
        // not an identifier
        return 0;
    
    int i = 1;
    while (!eof(i)) {
        char c = peek(i);
        if (!is_alphanum(c) && c != '_')
            return i;
        
        ++i;
    }
    
    return i;
}

int Lexer::read_constant() {
    int i = 0;
    while (is_num(peek(i)))
        ++i;
    return i;
}
