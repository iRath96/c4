#include <iostream>
#include <fstream>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "Lexer.h"
#include "Parser.h"

#include "Analyzer.h"
#include "Beautifier.h"
#include "Compiler.h"

bool debug_mode = false;
bool enable_output = true;
bool do_sema = true;
bool do_compile = true;

const char *token_kind_name(Token::Kind kind) {
	using Kind = Token::Kind;

	switch (kind) {
		case Kind::KEYWORD: return "keyword";
		case Kind::IDENTIFIER: return "identifier";
		case Kind::CONSTANT: return "constant";
		case Kind::STRING_LITERAL: return "string-literal";
		case Kind::PUNCTUATOR: return "punctuator";
		case Kind::END: return "end";
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
			Token t;
			lexer.next(&t);
			if (t.kind == Token::Kind::END) break;

			enable_output &&
			printf("%s:%d:%d: %s %s\n", filename, t.pos.line, t.pos.column, token_kind_name(t.kind), t.text.c_str());
		}
	} catch (Lexer::Error e) {
		fprintf(stderr, "%s:%d:%d: error: %s\n", filename, e.end_pos.line, e.end_pos.column, e.message.c_str());
		exit(1);
	}
}

void parse(const char *filename, bool printAST) {
	Lexer lexer = create_lexer(filename);
	Parser parser(&lexer);
	Buffer<Parser::Output> buffer(&parser);

	Analyzer analyzer(buffer.createChild());
	Compiler compiler(&analyzer);

	try {
		if (do_compile) compiler.drain();
		else if (do_sema) analyzer.drain();
		else buffer.drain();
	} catch (Lexer::Error e) {
		fprintf(stderr, "%s:%d:%d: error: %s\n", filename, e.end_pos.line, e.end_pos.column, e.message.c_str());
		exit(1);
	} catch (ParserError e) {
		if (debug_mode) {
			parser.print_debug_tree();
			parser.print_context();
		}

		fprintf(stderr, "%s:%d:%d: error: %s\n", filename, e.pos.line, e.pos.column, e.message.c_str());
		exit(1);
	} catch (AnalyzerError e) {
		fprintf(stderr, "%s:%d:%d: error: %s\n", filename, e.pos.line, e.pos.column, e.message.c_str());
		exit(1);
	}

	if (printAST || debug_mode) {
		Beautifier beautifier(buffer.createChild());
		beautifier.drain();
	}
}

int main(int argc, const char *argv[]) {
	for (int i = 1; i < argc; ++i) {
		if (!strcmp(argv[i], "--tokenize"))
			tokenize(argv[++i]);
		else if (!strcmp(argv[i], "--debug"))
			debug_mode = true;
		else if (!strcmp(argv[i], "--no-sema"))
			do_sema = false;
		else if (!strcmp(argv[i], "--dry"))
			enable_output = false;
		else if (!strcmp(argv[i], "--parse"))
			parse(argv[++i], false);
		else if (!strcmp(argv[i], "--print-ast"))
			parse(argv[++i], true);
		else {
			printf("Unrecognized argument: %s\n", argv[i]);
			exit(1);
		}
	}
}
