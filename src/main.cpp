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
#include "FileSink.h"
#include "JITEngine.h"
#include "Optimizer.h"


bool debug_mode = false;
bool enable_output = true;
bool do_sema = true;
enum {
	TOKENIZE, PARSE, PRINT_AST, COMPILE
} mode = COMPILE;

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

class FileSource : public Source<std::string> {
protected:
	std::string filename;
	bool hasFinished = false;

public:
	FileSource(std::string filename) : filename(filename) {}

	virtual bool next(std::string *output) {
		if (hasFinished) return false;

		FILE *f = fopen(filename.c_str(), "rb"); // @todo use fstream
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

		*output = std::string(buffer, length);

		hasFinished = true;
		return true;
	}
};

class REPLSource : public Source<std::string> {
public:
	Parser *parser;
	
	REPLSource() {}

	virtual bool next(std::string *output) {
		std::cout << ((parser && parser->depth > 1) ? "... " : ">>> ") << std::flush;
		std::getline(std::cin, *output);
		return !output->empty();
	}
};

void parse(const char *filename) {
	std::string name(filename);
	size_t i = name.rfind('/') + 1, j = name.rfind('.');
	std::string llPath = name.substr(i, j - i) + ".ll";
	name = name.substr(i);

	FileSource source(filename);
	Lexer lexer(&source);
	Parser parser(&lexer);
	Buffer<Parser::Output> buffer(&parser);

	Analyzer analyzer(buffer.createChild());
	Compiler compiler(&analyzer, name);
	Optimizer optimizer(&compiler, compiler.modPtr.get());
	FileSink output(&optimizer, compiler.modPtr.get(), llPath, debug_mode);

	try {
		if (mode == COMPILE) output.drain();
		else if (mode == TOKENIZE) {
			while (true) {
				Token t;
				if (!lexer.next(&t)) break;

				enable_output &&
				printf("%s:%d:%d: %s %s\n", filename, t.pos.line, t.pos.column, token_kind_name(t.kind), t.text.c_str());
			}
		} else if (do_sema) analyzer.drain();
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

	if (mode == PRINT_AST) {
		Beautifier beautifier(buffer.createChild());
		beautifier.drain();
	}
}

void repl() {
	REPLSource source;
	Lexer lexer(&source);
	Parser parser(&lexer, true);
	Buffer<Parser::Output> buffer(&parser);

	Analyzer analyzer(buffer.createChild());
	Compiler compiler(&analyzer, "repl");
	JITEngine jit(&compiler, &compiler); // @todo style

	source.parser = &parser;

	while (true) {
		try { // @todo not DRY
			jit.next(nullptr);
		} catch (Lexer::Error e) { // @todo style mismatch with ParserError/AnalyzerError
			fprintf(stderr, "stdin:%d:%d: error: %s\n", e.end_pos.line, e.end_pos.column, e.message.c_str());
		} catch (ParserError e) {
			if (debug_mode) {
				parser.print_debug_tree();
				parser.print_context();
			}

			fprintf(stderr, "stdin:%d:%d: error: %s\n", e.pos.line, e.pos.column, e.message.c_str());
			parser.reset();
		} catch (AnalyzerError e) {
			fprintf(stderr, "stdin:%d:%d: error: %s\n", e.pos.line, e.pos.column, e.message.c_str());
		}
	}
}

int main(int argc, const char *argv[]) {
	for (int i = 1; i < argc; ++i) {
		if (!strcmp(argv[i], "--tokenize")) mode = TOKENIZE;
		else if (!strcmp(argv[i], "--debug")) debug_mode = true;
		else if (!strcmp(argv[i], "--no-sema")) do_sema = false;
		else if (!strcmp(argv[i], "--dry")) enable_output = false;
		else if (!strcmp(argv[i], "--parse")) mode = PARSE;
		else if (!strcmp(argv[i], "--print-ast")) mode = PRINT_AST;
		else if (!strcmp(argv[i], "--repl")) repl();
		else if (!strcmp(argv[i], "--compile")) mode = COMPILE;
		else parse(argv[i]);
	}
}
