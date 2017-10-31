//
//  Lexer.cpp
//  c4
//
//  Created by Alexander Rath on 26.10.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include "Lexer.h"

#define TEST_CHAR(lookup, chr) lookup[(int)(unsigned char)chr]

bool is_whitespace[256] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

bool is_alpha[256] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };

bool is_octal(char c) {
    return c >= '0' && c <= '7';
}

bool is_decimal[256] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

bool is_hexadecimal(char c) {
    return TEST_CHAR(is_decimal, c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

bool is_alphanum[256] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };

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
        else if ((length = read_string()) || (length = read_char()))
            return create_token(TokenType::STRING_LITERAL, length);
        else if ((length = read_identifier()))
            return create_token(TokenType::IDENTIFIER, length);
        else if ((length = read_constant()))
            return create_token(TokenType::CONSTANT, length);
        
        // no token was found
        error("unrecognized character", 0);
    }
    
    return create_token(TokenType::END, 0);
}

Token Lexer::create_token(TokenType type, int length) {
    Token token(input.data + pos.index, length);
    token.pos = pos;
    token.type = type;
    
    consume(length);
    
    if (token.type == TokenType::IDENTIFIER) {
#define K(x, y) if (length == y && !strcmp(token.text, x)) token.type = TokenType::KEYWORD; else
        switch (token.text[0]) {
            case 'a': K("auto", 4) {}; break;
            case 'b': K("break", 5) {}; break;
            case 'c': K("case", 4) K("char", 4) K("const", 5) K("continue", 8) {}; break;
            case 'd': K("default", 7) K("do", 2) K("double", 6) {}; break;
            case 'e': K("else", 4) K("enum", 4) K("extern", 6) {}; break;
            case 'f': K("float", 5) K("for", 3) {}; break;
            case 'g': K("goto", 4) {}; break;
            case 'i': K("if", 2) K("inline", 6) K("int", 3) {}; break;
            case 'l': K("long", 4) {}; break;
            case 'r': K("register", 8) K("restrict", 8) K("return", 6) {}; break;
            case 's': K("short", 5) K("signed", 6) K("sizeof", 6) K("static", 6) K("struct", 6) K("switch", 6) {}; break;
            case 't': K("typedef", 7) {}; break;
            case 'u': K("union", 5) K("unsigned", 8) {}; break;
            case 'v': K("void", 4) K("volatile", 8) {}; break;
            case 'w': K("while", 5) {}; break;
            case '_':
                K("_Alignas", 8) K("_Alignof", 8) K("_Atomic", 7) K("_Bool", 5) K("_Complex", 8)
                K("_Generic", 8) K("_Imaginary", 10) K("_Noreturn", 9) K("_Static_assert", 14) K("_Thread_local", 13)
                {}; break;
        }
#undef K
    }
    
    return token;
}

int Lexer::read_whitespace() {
    int i = 0;
    while (TEST_CHAR(is_whitespace, peek(i)))
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
        i = read_escape_seq(i);
    }
    
    error("EOF encountered while reading string", i);
    return 0;
}

int Lexer::read_escape_seq(int i) {
    if (eof(i))
        // can't read anything
        return i;
    
    if (peek(i++) != '\\')
        // ordinary character of length 1
        return i;
    
    char c = peek(i++);
    if (c == '\'' || c == '"' || c == '?' || c == '\\')
        return i;
    
    if (is_octal(c)) {
        // octal escape sequence
        while (i <= 4 && is_octal(peek(i)))
            ++i;
        return i;
    }
    
    if (c == 'x') {
        if (!is_hexadecimal(peek(i)))
            error("hexadecimal escape sequence expected", i);
        
        while (is_hexadecimal(peek(++i)));
        return i;
    }
    
    if (c == 'a' || c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' || c == 'v')
        return i;
    
    error("invalid escape sequence", i - 1);
    return 0;
}

int Lexer::read_char() {
    if (peek(0) != '\'')
        // not a character
        return 0;
    
    if (peek(1) == '\'')
        error("empty character constant", 1);
    
    int i = read_escape_seq(1);
    
    if (eof(i))
        error("EOF encountered while reading character constant", i);
    
    if (peek(i) != '\'')
        error("character constant too long", i);
    
    return i + 1;
}

int Lexer::read_comment() {
    if (peek(0) != '/' || peek(1) != '*')
        // not a comment
        return 0;
    
    int i = 3;
    while (!eof(i)) {
        if (peek(i - 1) == '*' && peek(i) == '/')
            return i + 1;
        
        ++i;
    }
    
    error("EOF encountered while reading comment", i);
    return 0;
}

int Lexer::read_punctuator() {
    char c = peek(0), d = peek(1);
    
    // punctuators of length 4
    
    if (c == '%' && d == ':' && peek(2) == '%' && peek(3) == ':')
        return 4;
    
    // punctuators of length 3
    
    if (c == '.' && d == '.' && peek(2) == '.')
        return 3;
    
    // punctuators of length 2
    bool is_bitwise = c == '&' || c == '|' || c == '^' || c == '!'; // not ~, because there is no ~=
    bool is_arithmetic = c == '*' || c == '/' || c == '%' || c == '+' || c == '-';
    bool is_comparison = c == '<' || c == '>';
    
    if (c == '-' && d == '>') return 2;
    if ((c == '+' || c == '-' || c == '&' || c == '|' || c == '<' || c == '>' || c == '#')
        && d == c)
        return 2;
    
    if ((is_arithmetic || is_bitwise || is_comparison || c == '=') && d == '=')
        return 2;
    
    if (c == '<' && (d == ':' || d == '%')) return 2;
    if ((c == ':' || c == '%') && d == '>') return 2;
    if (c == '%' && d == ':') return 2;
    
    // punctuators of length 1
    
    if (c == '[' || c == ']' || c == '(' || c == ')' || c == '{' || c == '}') return 1; // brackets
    if (c == '#' || c == ',' || c == '=' || c == '?' || c == ':' || c == ';') return 1; // weird stuff
    if (is_arithmetic || is_bitwise || is_comparison || c == '~') return 1;
    if (c == '.') return 1; // subscript
    
    return 0;
}

int Lexer::read_identifier() {
    char c = peek(0);
    if (!TEST_CHAR(is_alpha, c) && c != '_')
        // not an identifier
        return 0;
    
    int i = 1;
    while (!eof(i)) {
        c = peek(i);
        if (!TEST_CHAR(is_alphanum, c) && c != '_')
            return i;
        
        ++i;
    }
    
    return i;
}

int Lexer::read_constant() {
    if (peek(0) == '0')
        // octal constants are now allowed
        return 1;
    
    int i = 0;
    while (TEST_CHAR(is_decimal, peek(i)))
        ++i;
    return i;
}

void Lexer::error(const std::string &message, int offset) {
    TextPosition start_pos = pos;
    consume(offset);
    throw LexerError(message, start_pos, pos);
}
