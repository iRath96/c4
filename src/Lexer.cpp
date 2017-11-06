//
//  Lexer.cpp
//  c4
//
//  Created by Alexander Rath on 26.10.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include "Lexer.h"
#include <ctype.h>

#define TEST_CHAR(lookup, chr) lookup[(int)(unsigned char)chr]

bool is_alpha[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0,
    1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
    1, 1, 1, 1, 1, 1
};

bool is_alphanum[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0,
    1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
    1, 1, 1, 1, 1, 1
};

bool is_octal(char c) {
    return c >= '0' && c <= '7';
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
    Token token;
    token.pos = pos;
    token.type = type;
    token.text = NULL;
    
    const char *raw_text = input.data.get() + pos.index;
    
    if (token.type == TokenType::IDENTIFIER) {
#define K(x, y, z) if (length == z && !strncmp(raw_text, x, length)) {\
    token.type = TokenType::KEYWORD; \
    token.text = x; \
    token.keyword = TokenKeyword::y; \
} else
        switch (raw_text[0]) {
            case 'a': K("auto", AUTO, 4) {}; break;
            case 'b': K("break", BREAK, 5) {}; break;
            case 'c': K("case", CASE, 4) K("char", CHAR, 4) K("const", CONST, 5) K("continue", CONTINUE, 8) {}; break;
            case 'd': K("default", DEFAULT, 7) K("do", DO, 2) K("double", DOUBLE, 6) {}; break;
            case 'e': K("else", ELSE, 4) K("enum", ENUM, 4) K("extern", EXTERN, 6) {}; break;
            case 'f': K("float", FLOAT, 5) K("for", FOR, 3) {}; break;
            case 'g': K("goto", GOTO, 4) {}; break;
            case 'i': K("if", IF, 2) K("inline", INLINE, 6) K("int", INT, 3) {}; break;
            case 'l': K("long", LONG, 4) {}; break;
            case 'r': K("register", REGISTER, 8) K("restrict", RESTRICT, 8) K("return", RETURN, 6) {}; break;
            case 's':
                K("short", SHORT, 5) K("signed", SIGNED, 6) K("sizeof", SIZEOF, 6)
                K("static", STATIC, 6) K("struct", STRUCT, 6) K("switch", SWITCH, 6) {}; break;
            case 't': K("typedef", TYPEDEF, 7) {}; break;
            case 'u': K("union", UNION, 5) K("unsigned", UNSIGNED, 8) {}; break;
            case 'v': K("void", VOID, 4) K("volatile", VOLATILE, 8) {}; break;
            case 'w': K("while", WHILE, 5) {}; break;
            case '_':
                K("_Alignas", _ALIGNAS, 8) K("_Alignof", _ALIGNOF, 8) K("_Atomic", _ATOMIC, 7)
                K("_Bool", _BOOL, 5) K("_Complex", _COMPLEX, 8) K("_Generic", _GENERIC, 8)
                K("_Imaginary", _IMAGINARY, 10) K("_Noreturn", _NORETURN, 9) K("_Static_assert", _STATIC_ASSERT, 14)
                K("_Thread_local", _THREAD_LOCAL, 13)
                {}; break;
        }
#undef K
    } else if (token.type == TokenType::PUNCTUATOR) {
        token.punctuator = last_punctuator;
    }
    
    if (token.text == NULL)
        token.text = find_text(raw_text, length);
    
    consume(length);
    
    return token;
}

int Lexer::read_whitespace() {
    int i = 0;
    while (isspace(peek(i)))
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
    
    if (peek(i) == '\n') {
        // newlines are not allowed in string-literals
        error("newline inside string-literal", i);
        return 0;
    }
    
    if (peek(i++) != '\\')
        // ordinary character of length 1
        return i;
    
    char c = peek(i++);
    if (c == '\'' || c == '"' || c == '?' || c == '\\' ||
        c == 'a' || c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' || c == 'v')
        return i;
    
    if (is_octal(c)) {
        int max_i = i + 2; // two more octal digits are allowed
        
        // octal escape sequence
        while (i <= max_i && is_octal(peek(i)))
            ++i;
        return i;
    }
    
    if (c == 'x') {
        if (!isxdigit(peek(i)))
            error("hexadecimal escape sequence expected", i);
        
        while (isxdigit(peek(++i)));
        return i;
    }
    
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
    if (peek(0) != '/')
        // not a comment
        return 0;
    
    int i;
    if (peek(1) == '*') {
        // block comment
        
        i = 3;
        while (!eof(i))
            if (peek(i - 1) == '*' && peek(i) == '/')
                return i + 1;
            else
                ++i;
    } else if (peek(1) == '/') {
        // single line comment
        
        i = 2;
        while (!eof(i))
            if (peek(i) == '\n')
                return i;
            else
                ++i;
    } else
        return 0;
    
    error("EOF encountered while reading comment", i);
    return 0;
}

int Lexer::read_punctuator() {
#define RET(x, y) { last_punctuator = TokenPunctuator::x; return y; }
    
    char c = peek(0), d = peek(1);
    
    // punctuators of length 4
    
    if (c == '%' && d == ':' && peek(2) == '%' && peek(3) == ':')
        RET(DOUBLE_HASH, 4)
    
    // punctuators of length 3
    
    if (c == '.' && d == '.' && peek(2) == '.') RET(ELIPSES, 3)
    if (c == '>' && d == '>' && peek(2) == '=') RET(RSHIFT_ASSIGN, 3)
    if (c == '<' && d == '<' && peek(2) == '=') RET(LSHIFT_ASSIGN, 3)
    
    // punctuators of length 2
    
#define P(x, y, z) if (c == x && d == y) RET(z, 2)
    P('#', '#', DOUBLE_HASH)
    
    P('+', '+', PLUSPLUS) P('-', '-', MINUSMINUS)
    P('&', '&', LOG_AND) P('|', '|', LOG_OR)
    P('<', '<', LSHIFT) P('>', '>', RSHIFT)
    P('<', '=', CMP_LTE) P('>', '=', CMP_GTE) P('=', '=', CMP_EQ) P('!', '=', CMP_NEQ)
    
    P('+', '=', PLUS_ASSIGN) P('-', '=', MINUS_ASSIGN)
    P('*', '=', MUL_ASSIGN) P('/', '=', DIV_ASSIGN)
    P('%', '=', MODULO_ASSIGN)
        
    P('&', '=', BIT_AND_ASSIGN) P('|', '=', BIT_OR_ASSIGN) P('^', '=', BIT_XOR_ASSIGN)
        
    P('-', '>', ARROW)
    
    P('<', '%', CB_OPEN)
    P('%', '>', CB_CLOSE)
    
    P('<', ':', SB_OPEN)
    P(':', '>', SB_CLOSE)
    
    P('%', ':', HASH)
#undef P
    
    // punctuators of length 1
    
#define P(x, y) case x: RET(y, 1)
    switch (c) {
        P('{', CB_OPEN) P('}', CB_CLOSE)
        P('[', SB_OPEN) P(']', SB_CLOSE)
        P('(', RB_OPEN) P(')', RB_CLOSE)
        P('<', AB_OPEN) P('>', AB_CLOSE)
            
        P('.', PERIOD) P('#', HASH) P(',', COMMA) P('=', ASSIGN) P('?', QMARK) P(':', COLON) P(';', SEMICOLON)
            
        P('+', PLUS) P('-', MINUS) P('*', ASTERISK) P('/', SLASH) P('%', MODULO)
            
        P('&', BIT_AND) P('|', BIT_OR) P('^', BIT_XOR) P('~', BIT_NOT)
        P('!', LOG_NOT)
    }
#undef P
    
    return 0;
    
#undef RET
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
    while (isdigit(peek(i)))
        ++i;
    return i;
}

void Lexer::error(const std::string &message, int offset) {
    TextPosition start_pos = pos;
    consume(offset);
    throw LexerError(message, start_pos, pos);
}
