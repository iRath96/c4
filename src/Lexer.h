//
//  Lexer.hpp
//  c4
//
//  Created by Alexander Rath on 26.10.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#ifndef Lexer_hpp
#define Lexer_hpp

#include <string>
#include <unordered_map>
#include <vector>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <memory>

struct TextPosition {
    int index = 0;
    int line = 1;
    int column = 1;
};

enum TokenKeyword {
    NOT_A_KEYWORD = 0,
    
    AUTO,
    BREAK,
    CASE, CHAR, CONST, CONTINUE,
    DEFAULT, DO, DOUBLE,
    ELSE, ENUM, EXTERN,
    FLOAT, FOR,
    GOTO,
    IF, INLINE, INT,
    LONG,
    REGISTER, RESTRICT, RETURN,
    SHORT, SIGNED, SIZEOF, STATIC, STRUCT, SWITCH,
    TYPEDEF,
    UNION, UNSIGNED,
    VOID, VOLATILE,
    WHILE,
    
    _ALIGNAS, _ALIGNOF, _ATOMIC, _BOOL, _COMPLEX,
    _GENERIC, _IMAGINARY, _NORETURN, _STATIC_ASSERT, _THREAD_LOCAL
};

enum Precedence {
    NONE = 15,
    
    ASSIGNMENT     = 13,
    CONDITIONAL    = 12,
    LOGICAL_OR     = 11,
    LOGICAL_AND    = 10,
    INCLUSIVE_OR   = 9,
    EXCLUSIVE_OR   = 8,
    AND            = 7,
    EQUALITY       = 6,
    RELATIONAL     = 5,
    SHIFT          = 4,
    ADDITIVE       = 3,
    MULTIPLICATIVE = 2,
    
    UNARY = 1,
    MAX   = 0
};

#define PRECEDENCE(punctuator) ((Precedence)(char)punctuator)
#define ID(name, id, precedence, token) name = (id << 8) | precedence,
enum TokenPunctuator {
    ID(NOT_A_PUNCTUATOR, 0, Precedence::NONE, "+")
    
    ID(CB_OPEN,  10, Precedence::NONE, "{")
    ID(CB_CLOSE, 11, Precedence::NONE, "}")
    ID(SB_OPEN,  12, Precedence::NONE, "[")
    ID(SB_CLOSE, 13, Precedence::NONE, "]")
    ID(RB_OPEN,  14, Precedence::NONE, "(")
    ID(RB_CLOSE, 15, Precedence::NONE, ")")
    
    ID(QMARK, 16, Precedence::CONDITIONAL, "?")
    ID(COLON, 17, Precedence::NONE, ":")
    
    ID(PLUS_ASSIGN,    20, Precedence::ASSIGNMENT, "+=")
    ID(MINUS_ASSIGN,   21, Precedence::ASSIGNMENT, "-=")
    ID(MUL_ASSIGN,     22, Precedence::ASSIGNMENT, "*=")
    ID(DIV_ASSIGN,     23, Precedence::ASSIGNMENT, "/=")
    ID(MODULO_ASSIGN,  24, Precedence::ASSIGNMENT, "%=")
    ID(BIT_OR_ASSIGN,  25, Precedence::ASSIGNMENT, "|=")
    ID(BIT_AND_ASSIGN, 26, Precedence::ASSIGNMENT, "&=")
    ID(BIT_XOR_ASSIGN, 27, Precedence::ASSIGNMENT, "^=")
    ID(RSHIFT_ASSIGN,  28, Precedence::ASSIGNMENT, ">>=")
    ID(LSHIFT_ASSIGN,  29, Precedence::ASSIGNMENT, "<<=")
    ID(ASSIGN,         30, Precedence::ASSIGNMENT, "=")
    
    ID(LOG_OR,  40, Precedence::LOGICAL_OR,   "||")
    ID(LOG_AND, 41, Precedence::LOGICAL_AND,  "&&")
    ID(BIT_OR,  42, Precedence::INCLUSIVE_OR, "|")
    ID(BIT_XOR, 43, Precedence::EXCLUSIVE_OR, "^")
    ID(BIT_AND, 44, Precedence::AND,          "&")
    
    ID(CMP_EQ,   50, Precedence::EQUALITY, "==")
    ID(CMP_NEQ,  51, Precedence::EQUALITY, "!=")
    
    ID(AB_OPEN,  60, Precedence::RELATIONAL, "<")
    ID(AB_CLOSE, 61, Precedence::RELATIONAL, ">")
    ID(CMP_LTE,  62, Precedence::RELATIONAL, "<=")
    ID(CMP_GTE,  63, Precedence::RELATIONAL, ">=")
    
    ID(RSHIFT, 70, Precedence::SHIFT, ">>")
    ID(LSHIFT, 71, Precedence::SHIFT, ">>")
    
    ID(PLUS,  80, Precedence::ADDITIVE, "+")
    ID(MINUS, 81, Precedence::ADDITIVE, "-")
    
    ID(ASTERISK, 90, Precedence::MULTIPLICATIVE, "*")
    ID(SLASH,    91, Precedence::MULTIPLICATIVE, "/")
    ID(MODULO,   92, Precedence::MULTIPLICATIVE, "%")
    
    ID(PLUSPLUS,   100, Precedence::UNARY, "++")
    ID(MINUSMINUS, 101, Precedence::UNARY, "--")
    ID(BIT_NOT,    102, Precedence::UNARY, "~")
    ID(LOG_NOT,    103, Precedence::UNARY, "!")
    
    ID(DOUBLE_HASH, 110, Precedence::NONE, "##")
    ID(HASH,        111, Precedence::NONE, "#")
    ID(ELIPSES,     112, Precedence::NONE, "...")
    ID(PERIOD,      113, Precedence::NONE, ".")
    ID(COMMA,       114, Precedence::NONE, ",")
    ID(SEMICOLON,   115, Precedence::NONE, ";")
    ID(ARROW,       116, Precedence::NONE, "->")
};
#undef ID

enum TokenType {
    KEYWORD, IDENTIFIER, CONSTANT, STRING_LITERAL, PUNCTUATOR,
    END
};

struct Token {
    TextPosition pos;
    
    TokenType type;
    const char *text;
    
    TokenKeyword keyword = TokenKeyword::NOT_A_KEYWORD;
    TokenPunctuator punctuator = TokenPunctuator::NOT_A_PUNCTUATOR;
};

struct LexerInput {
    std::shared_ptr<char> data;
    int length;
};

class LexerError {
public:
    std::string message;
    TextPosition start_pos, end_pos;
    
    LexerError(const std::string &message, TextPosition start_pos, TextPosition end_pos)
    : message(message), start_pos(start_pos), end_pos(end_pos) {
        
    }
};

struct StringWithLength {
    const char *data;
    int length;
    
    StringWithLength(const char *data, int length) : data(data), length(length) {}
    
    bool operator<(const StringWithLength &other) const {
        if (length != other.length)
            return length < other.length;
        
        return strncmp(data, other.data, length) == -1;
    }
    
    bool operator==(const StringWithLength &other) const {
        return length == other.length && !strncmp(data, other.data, length);
    }
};

namespace std {
    template <>
    struct hash<StringWithLength> {
        std::size_t operator()(const StringWithLength &k) const {
            size_t hash = std::hash<int>()(k.length);
            hash ^= (k.data[0] * (k.length > 0)) << 2;
            hash ^= (k.data[1] * (k.length > 1)) << 3;
            hash ^= (k.data[2] * (k.length > 2)) << 5;
            hash ^= (k.data[3] * (k.length > 3)) << 7;
            return hash;
        }
    };
}

class Lexer {
public:
    Lexer(char *data, int length) {
        input.data = std::shared_ptr<char>(data);
        input.length = length;
        
        symbol_table.reserve(1024);
        
        replace_eol();
    }
    
    bool has_ended() {
        return eof(0);
    }
    
    Token next_token();
    
protected:
    std::unordered_map<StringWithLength, std::shared_ptr<char>> symbol_table;
    
    const char *find_text(const char *source, int length) {
        StringWithLength s(source, length);
        
        auto it = symbol_table.find(s);
        if (it != symbol_table.end())
            return it->second.get();
        
        char *buffer = (char *)malloc(length + 1);
        memcpy(buffer, source, length);
        buffer[length] = 0;
        
        //printf("Creating token %s\n", buffer);
        symbol_table[s] = std::shared_ptr<char>(buffer);
        
        return buffer;
    }
    
    LexerInput input;
    TextPosition pos;
    
    TokenPunctuator last_punctuator;
    
    void replace_eol() {
        bool last_char_was_cr = false;
        
        int skip = 0;
        for (int i = 0; i < input.length; ++i) {
            char c_in = input.data.get()[i + skip];
            char c_out = c_in;
            
            switch (c_in) {
                // @todo could as well replace other EOL characters with LF here.
                case '\r':
                    c_out = '\n';
                    break;
                case '\n':
                    if (last_char_was_cr) {
                        // don't replace CRLF with LFLF, replace it with one LF only
                        --input.length;
                        ++skip;
                        
                        if (i == input.length)
                            // CRLF at end of file, we're done
                            return;
                        
                        c_out = input.data.get()[i + skip];
                    }
            }
            
            input.data.get()[i] = c_out;
            last_char_was_cr = c_in == '\r';
        }
    }
    
    void consume(int length) {
        const char *buffer = input.data.get() + pos.index;
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
    }
    
    char peek(int offset) {
        if (eof(offset))
            return 0;
        
        return input.data.get()[pos.index + offset];
    }
    
    bool eof(int offset) {
        return pos.index + offset >= input.length;
    }
    
protected:
    Token create_token(TokenType type, int length);
    void error(const std::string &message, int offset);
    
    int read_whitespace();
    int read_comment();
    
    int read_punctuator();
    int read_identifier();
    
    int read_escape_seq(int);
    int read_string();
    int read_char();
    
    int read_constant();
};

#endif /* Lexer_hpp */
