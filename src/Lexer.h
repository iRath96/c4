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

enum TokenPunctuator {
    CB_OPEN /* { */, CB_CLOSE /* } */,
    SB_OPEN /* [ */, SB_CLOSE /* ] */,
    RB_OPEN /* ( */, RB_CLOSE /* ) */,
    AB_OPEN /* < */, AB_CLOSE /* > */,
    
    CMP_LTE /* <= */, CMP_GTE /* >= */, CMP_EQ /* == */, CMP_NEQ /* != */,
    
    DOUBLE_HASH /* ## */, HASH /* # */,
    ELIPSES /* ... */,
    
    SUBSCRIPT /* . */, COMMA /* , */, COLON /* : */, QMARK /* ? */, SEMICOLON /* ; */,
    
    ARROW /* -> */,
    
    PLUSPLUS /* ++ */, MINUSMINUS /* -- */,
    PLUS /* + */, MINUS /* - */, ASTERISK /* * */, SLASH /* / */, MODULO /* % */,
    
    BIT_AND /* & */, BIT_OR /* | */, BIT_XOR /* ^ */, BIT_NOT /* ~ */,
    LOG_AND /* && */, LOG_OR /* || */, LOG_NOT /* ! */,
    
    RSHIFT /* >> */, LSHIFT /* << */,
    
    PLUS_ASSIGN /* += */, MINUS_ASSIGN /* -= */, MUL_ASSIGN /* *= */, DIV_ASSIGN /* /= */, MODULO_ASSIGN /* %= */,
    BIT_OR_ASSIGN /* |= */, BIT_AND_ASSIGN /* &= */, BIT_XOR_ASSIGN /* ^= */,
    RSHIFT_ASSIGN /* >>= */, LSHIFT_ASSIGN /* <<= */,
    ASSIGN /* = */
};

enum TokenType {
    KEYWORD, IDENTIFIER, CONSTANT, STRING_LITERAL, PUNCTUATOR,
    END
};

struct Token {
    TextPosition pos;
    
    TokenType type;
    const char *text;
    
    union {
        TokenKeyword keyword;
        TokenPunctuator punctuator;
    } meta;
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
            size_t hash = k.length & 0xFFFF;
            hash |= (k.data[0] * (k.length > 0)) << 16;
            hash |= (k.data[1] * (k.length > 1)) << 20;
            hash |= (k.data[2] * (k.length > 2)) << 24;
            hash |= (k.data[3] * (k.length > 3)) << 28;
            return hash;
        }
    };
}

class Lexer {
public:
    Lexer(char *data, int length) {
        input.data = std::shared_ptr<char>(data);
        input.length = length;
        
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
