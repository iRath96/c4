#include "Lexer.h"
#include <ctype.h>

#define TEST_CHAR(lookup, chr) lookup[(int)(unsigned char)chr]

namespace lexer {

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

bool is_octal(char c) { return c >= '0' && c <= '7'; }

bool Lexer::next(Token *token) {
	using Kind = Token::Kind;

	peek(0); // make sure eof flag is set appropriately

	while (!eof(0)) {
		int length = 0;
		char c = peek(0);

		if (isspace(c)) {
			consume(read_whitespace());
			continue;
		} else if (c == '/' && (length = read_comment())) {
			// skip whitespace and comments
			consume(length);
			continue;
		} else if (c == '"')
			return create_token(Kind::STRING_LITERAL, read_string(token->stringValue), token);
		else if (c == '\'') {
			token->isChar = true;
			return create_token(Kind::CONSTANT, read_char(token->intValue), token);
		} else if (TEST_CHAR(is_alpha, c) || c == '_')
			return create_token(Kind::IDENTIFIER, read_identifier(), token);
		else if (isdigit(c))
			return create_token(Kind::CONSTANT, read_constant(token->intValue), token);
		else if ((length = read_punctuator()))
			return create_token(Kind::PUNCTUATOR, length, token);

		// no token was found
		error("unrecognized character", 0);
	}

	create_token(Kind::END, 0, token);
	return false;
}

bool Lexer::create_token(Token::Kind kind, int length, Token *token) {
	token->pos = pos;
	token->kind = kind;

	const char *raw_text = input.data.get() + pos.index;

	if (token->kind == Token::Kind::IDENTIFIER) {
#define K(x, y, z) if (length == z && !strncmp(raw_text, x, length)) {\
	token->kind = Token::Kind::KEYWORD; \
	token->text = x; \
	token->keyword = Token::Keyword::y; \
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
	} else if (token->kind == Token::Kind::PUNCTUATOR) {
		token->punctuator = last_punctuator;
	}

	if (token->text.empty())
		token->text = std::string(raw_text, length);

	consume(length);
	token->end_pos = pos;

	return true;
}

int Lexer::read_whitespace() {
	int i = 0;
	while (isspace(peek(i))) ++i;
	return i;
}

int Lexer::read_string(std::string &str) {
	if (peek(0) != '"') return 0; // not a string

	int i = 1;
	while (!eof(i)) {
		int output, c = peek(i);
		if (c == '"') return i + 1;
		i = read_escape_seq(i, output);
		str += (char)output; // @todo inefficient
	}

	error("EOF encountered while reading string", i);
	return 0;
}

char xval(char chr) {
	switch (chr) {
	case 'a': case 'A': return 10;
	case 'b': case 'B': return 11;
	case 'c': case 'C': return 12;
	case 'd': case 'D': return 13;
	case 'e': case 'E': return 14;
	case 'f': case 'F': return 15;
	}
	
	return chr - '0';
}

int Lexer::read_escape_seq(int i, int &output) {
	if (eof(i)) return i; // can't read anything

	if (peek(i) == '\n') {
		// newlines are not allowed in string-literals
		error("newline inside string-literal", i);
		return 0;
	}

	output = peek(i++);
	if (output != '\\') return i; // ordinary character of length 1

	output = peek(i++);
	switch (output) {
	case '\\': case '"': case '\'': return i;
	case '?': output = '\?'; return i;
	case 'a': output = '\a'; return i;
	case 'b': output = '\b'; return i;
	case 'f': output = '\f'; return i;
	case 'n': output = '\n'; return i;
	case 'r': output = '\r'; return i;
	case 't': output = '\t'; return i;
	case 'v': output = '\v'; return i;
	}

	if (is_octal(output)) {
		output -= '0';

		int max_i = i + 2; // two more octal digits are allowed

		// octal escape sequence
		while (i < max_i && is_octal(peek(i))) {
			output = output * 8 + (peek(i) - '0');
			++i;
		}

		return i;
	}

	if (output == 'x') {
		output = 0;

		if (!isxdigit(peek(i))) error("hexadecimal escape sequence expected", i);
		while (isxdigit(peek(++i)))
			output = output * 16 + xval(peek(i));

		return i;
	}

	error("invalid escape sequence", i - 1);
	return 0;
}

int Lexer::read_char(int &output) {
	if (peek(0) != '\'')
		// not a character
		return 0;

	if (peek(1) == '\'') error("empty character constant", 1);

	int i = read_escape_seq(1, output);

	if (eof(i)) error("EOF encountered while reading character constant", i);
	if (peek(i) != '\'') error("character constant too long", i);

	return i + 1;
}

int Lexer::read_comment() {
	if (peek(0) != '/') return 0; // not a comment

	int i;
	if (peek(1) == '*') {
		// block comment

		i = 3;
		while (!eof(i))
			if (peek(i - 1) == '*' && peek(i) == '/') return i + 1;
			else ++i;
	} else if (peek(1) == '/') {
		// single line comment

		i = 2;
		while (!eof(i))
			if (peek(i) == '\n') return i;
			else ++i;
	} else
		return 0;

	error("EOF encountered while reading comment", i);
	return 0;
}

int Lexer::read_punctuator() {
#define RET(x, y) { last_punctuator = Token::Punctuator::x; return y; }

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
	if (!TEST_CHAR(is_alpha, c) && c != '_') return 0; // not an identifier

	int i = 1;
	while (!eof(i)) {
		c = peek(i);
		if (!TEST_CHAR(is_alphanum, c) && c != '_') return i;
		++i;
	}

	return i;
}

int Lexer::read_constant(int &output) {
	output = 0;
	if (peek(0) == '0') return 1; // octal constants are now allowed

	int i = 0;
	while (isdigit(peek(i))) {
		output = output * 10 + (peek(i) - '0');
		++i;
	}

	return i;
}

void Lexer::error(const std::string &message, int offset) {
	TextPosition start_pos = pos;
	consume(offset);
	throw Error(message, start_pos, pos);
}

void Lexer::replace_eol() {
	bool last_char_was_cr = false;

	int skip = 0;
	for (int i = 0; i < input.length; ++i) {
		char c_in = input.data.get()[i + skip];
		char c_out = c_in;

		switch (c_in) {
			case '\r':
				c_out = '\n';
				break;

			case '\n':
				if (last_char_was_cr) {
					// don't replace CRLF with LFLF, replace it with one LF only
					--input.length;
					++skip;

					if (i == input.length) return; // CRLF at end of file, we're done
					c_out = input.data.get()[i + skip];
				}
		}

		input.data.get()[i] = c_out;
		last_char_was_cr = c_in == '\r';
	}
}

void Lexer::consume(int length) {
	const char *buffer = input.data.get() + pos.index;
	pos.index += length;

	for (int i = 0; i < length; ++i) {
		// update position
		if (buffer[i] == '\n') {
			++pos.line;
			pos.column = 1;
		} else
			++pos.column;
	}
}

}
