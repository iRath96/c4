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

namespace lexer {

struct TextPosition {
	int index  = 0;
	int line   = 1;
	int column = 1;
};

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

	enum class Kind : uint8_t {
		KEYWORD, IDENTIFIER, CONSTANT, STRING_LITERAL, PUNCTUATOR,
		END
	};

	TextPosition pos, end_pos;

	std::string text;

	Kind kind;
	Keyword keyword = Keyword::NOT_A_KEYWORD;
	Punctuator punctuator = Punctuator::NOT_A_PUNCTUATOR;
};

class Lexer : public Source<Token> {
public:
	struct Input {
		std::shared_ptr<char> data;
		int length;
	};

	class Error {
	public:
		std::string message;
		TextPosition start_pos, end_pos;

		Error(const std::string &message, TextPosition start_pos, TextPosition end_pos)
		: message(message), start_pos(start_pos), end_pos(end_pos) {}
	};

	Lexer(char *data, int length) {
		input.data = std::shared_ptr<char>(data);
		input.length = length;

		replace_eol();
	}

	virtual bool next(Token *);

private:
	Input input;
	TextPosition pos;

	Token::Punctuator last_punctuator;

	void replace_eol();
	void consume(int length);

	__attribute__((always_inline)) inline char peek(int offset) const {
		if (eof(offset)) return 0;
		return input.data.get()[pos.index + offset];
	}

	__attribute__((always_inline)) inline bool eof(int offset) const {
		return pos.index + offset >= input.length;
	}

	bool create_token(Token::Kind kind, int length, Token *token);
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

}

#endif /* Lexer_hpp */
