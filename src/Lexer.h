#ifndef Lexer_hpp
#define Lexer_hpp

#include <string>
#include <unordered_map>
#include <vector>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <memory>

#include "streams.h"
#include "common.h"

namespace lexer {

struct Token {
	enum class Precedence : uint8_t {
		NONE = 15,

		ASSIGNMENT   = 13,
		CONDITIONAL	 = 12,
		LOGICAL_OR   = 11,
		LOGICAL_AND  = 10,
		INCLUSIVE_OR = 9,
		EXCLUSIVE_OR = 8,
		AND          = 7,
		EQUALITY     = 6,
		RELATIONAL   = 5,
		SHIFT        = 4,
		ADDITIVE     = 3,
		MULTIPLICATIVE = 2,

		UNARY = 1,
		MAX   = 0
	};

#define ID(name, id, precedence, token) name = (id << 8) | (int)precedence,
	enum class Punctuator : uint32_t {
		ID(NOT_A_PUNCTUATOR, 0, Precedence::NONE, "")
		ID(NEVER, 120, Precedence::NONE, "")

		ID(CB_OPEN,  10, Precedence::NONE, "{")
		ID(CB_CLOSE, 11, Precedence::NONE, "}")
		ID(SB_OPEN,  12, Precedence::NONE, "[")
		ID(SB_CLOSE, 13, Precedence::NONE, "]")
		ID(RB_OPEN,  14, Precedence::NONE, "(")
		ID(RB_CLOSE, 15, Precedence::NONE, ")")

		ID(QMARK, 16, Precedence::CONDITIONAL, "?")
		ID(COLON, 17, Precedence::NONE, ":")

		ID(PLUS_ASSIGN,  20, Precedence::ASSIGNMENT, "+=")
		ID(MINUS_ASSIGN, 21, Precedence::ASSIGNMENT, "-=")
		ID(MUL_ASSIGN,	 22, Precedence::ASSIGNMENT, "*=")
		ID(DIV_ASSIGN,	 23, Precedence::ASSIGNMENT, "/=")
		ID(MODULO_ASSIGN,  24, Precedence::ASSIGNMENT, "%=")
		ID(BIT_OR_ASSIGN,  25, Precedence::ASSIGNMENT, "|=")
		ID(BIT_AND_ASSIGN, 26, Precedence::ASSIGNMENT, "&=")
		ID(BIT_XOR_ASSIGN, 27, Precedence::ASSIGNMENT, "^=")
		ID(RSHIFT_ASSIGN,  28, Precedence::ASSIGNMENT, ">>=")
		ID(LSHIFT_ASSIGN,  29, Precedence::ASSIGNMENT, "<<=")
		ID(ASSIGN, 30, Precedence::ASSIGNMENT, "=")

		ID(LOG_OR,  40, Precedence::LOGICAL_OR,   "||")
		ID(LOG_AND, 41, Precedence::LOGICAL_AND,  "&&")
		ID(BIT_OR,  42, Precedence::INCLUSIVE_OR, "|")
		ID(BIT_XOR, 43, Precedence::EXCLUSIVE_OR, "^")
		ID(BIT_AND, 44, Precedence::AND,		  "&")

		ID(CMP_EQ,   50, Precedence::EQUALITY, "==")
		ID(CMP_NEQ,  51, Precedence::EQUALITY, "!=")

		ID(AB_OPEN,  60, Precedence::RELATIONAL, "<")
		ID(AB_CLOSE, 61, Precedence::RELATIONAL, ">")
		ID(CMP_LTE,  62, Precedence::RELATIONAL, "<=")
		ID(CMP_GTE,  63, Precedence::RELATIONAL, ">=")

		ID(RSHIFT, 70, Precedence::SHIFT, ">>")
		ID(LSHIFT, 71, Precedence::SHIFT, "<<")

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
		ID(HASH,      111, Precedence::NONE, "#")
		ID(ELIPSES,   112, Precedence::NONE, "...")
		ID(PERIOD,    113, Precedence::NONE, ".")
		ID(COMMA,     114, Precedence::NONE, ",")
		ID(SEMICOLON, 115, Precedence::NONE, ";")
		ID(ARROW,     116, Precedence::NONE, "->")
	};
#undef ID

	static inline Precedence precedence(Punctuator punctuator) {
		return (Precedence)(char)punctuator;
	}

	enum class Keyword : uint8_t {
		NOT_A_KEYWORD = 0,

		AUTO, BREAK, CASE, CHAR, CONST, CONTINUE,
		DEFAULT, DO, DOUBLE, ELSE, ENUM, EXTERN,
		FLOAT, FOR, GOTO, IF, INLINE, INT, LONG,
		REGISTER, RESTRICT, RETURN,
		SHORT, SIGNED, SIZEOF, STATIC, STRUCT, SWITCH,
		TYPEDEF, UNION, UNSIGNED, VOID, VOLATILE, WHILE,

		_ALIGNAS, _ALIGNOF, _ATOMIC, _BOOL, _COMPLEX,
		_GENERIC, _IMAGINARY, _NORETURN, _STATIC_ASSERT, _THREAD_LOCAL
	};

	static const char *operatorName(Punctuator punctuator);
	static const char *keywordName(Keyword keyword);

	enum class Kind : uint8_t {
		KEYWORD, IDENTIFIER, CONSTANT, STRING_LITERAL, PUNCTUATOR,
		END
	};

	common::TextPosition pos, end_pos;

	std::string text;

	Kind kind;
	Keyword keyword = Keyword::NOT_A_KEYWORD;
	Punctuator punctuator = Punctuator::NOT_A_PUNCTUATOR;

	// @todo not elegant
	std::string stringValue;
	int intValue;
	bool isChar = false;
};

class LexerError : public common::Error {
public:
	LexerError(const std::string &message, common::TextPosition pos)
	: common::Error(message, pos) {}

	[[noreturn]] virtual void raise() { throw *this; }
};

/**
 * When asked for a fragment, requests data from its source and tries to extract
 * a single token from it. The remaining data (not part of the token) is buffered.
 * @note It would be more elegant to request single characters from the source and
 *       have some rewind functionality -- but sadly, this would also be a bit less
 *       efficient.
 */
class Lexer : public streams::Stream<std::string, Token> {
public:
	Lexer(Source<std::string> *source) : Stream<std::string, Token>(source) {}
	virtual bool next(Token *);

protected:
	std::string buffer;
	common::TextPosition pos;

	Token::Punctuator last_punctuator;

	bool acquire() {
		std::string data;
		if (!this->source->next(&data)) return false;

		replace_eol(data);
		buffer += data;
		return true;
	}

	bool last_char_was_cr = false;
	void replace_eol(std::string &);
	void consume(int length);

	__attribute__((always_inline)) inline char peek(int offset) const {
		if (eof(offset)) return 0;
		return buffer[pos.index + offset];
	}

	__attribute__((always_inline)) inline bool eof(int offset) const {
		return pos.index + offset >= buffer.length();
	}

	bool create_token(Token::Kind kind, int length, Token *token);
	void error(const std::string &message, int offset);

	int read_whitespace();
	int read_comment();

	int read_punctuator();
	int read_identifier();

	int read_escape_seq(int, int &);
	int read_string(std::string &);
	int read_char(int &);

	int read_constant(int &);
};

}

#endif /* Lexer_hpp */
