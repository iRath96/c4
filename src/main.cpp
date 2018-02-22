#include <iostream>
#include <fstream>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "Lexer.h"
#include "Parser.h"

#include "common.h"
#include "Analyzer.h"
#include "Beautifier.h"
#include "IRGenerator.h"
#include "FileSink.h"
#include "JITEngine.h"
#include "Optimizer.h"


using namespace streams;
using namespace std;

using namespace lexer;
using namespace parser;
using namespace compiler;
using namespace optimizer;
using namespace utils;


bool debug_mode = false;
bool enable_output = true;
bool do_sema = true;

bool hasOpt = false;
Optimizer::Options opt;

enum {
	TOKENIZE, PARSE, PRINT_AST, COMPILE, OPTIMIZE
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

/**
 * When first requested to yield a fragment, reads the entire file
 * and returns its contents. Subsequent fragment requests will return
 * failure.
 * @todo This should be changed so that the file is read in chunks.
 */
class FileSource : public Source<string> {
protected:
	string filename;
	bool hasFinished = false;

public:
	FileSource(string filename) : filename(filename) {}

	virtual bool next(string *output) {
		if (hasFinished) return false;

		FILE *f = fopen(filename.c_str(), "rb"); // @todo use fstream
		if (!f) {
			cerr << "Could not open " << filename << " for reading." << endl;
			exit(1);
		}

		fseek(f, 0, SEEK_END);
		int length = (int)ftell(f);

		char *buffer = (char *)malloc(length);
		fseek(f, 0, SEEK_SET);

		if (length && fread(buffer, length, 1, f) != 1) {
			cerr << "fread for " << filename << " failed." << endl;
			exit(1);
		}

		fclose(f);

		*output = string(buffer, length);
		free(buffer);

		hasFinished = true;
		return true;
	}
};

/**
 * When asked to yield a fragment, reads a line from stdin and returns it.
 */
class REPLSource : public Source<string> {
public:
	Parser *parser;
	
	REPLSource() {}

	virtual bool next(string *output) {
		cout << ((parser && parser->depth > 1) ? "... " : ">>> ") << flush;
		getline(cin, *output);
		return !output->empty();
	}
};

void parse(const char *filename) {
	bool do_optimize = false;
	if (mode == OPTIMIZE || hasOpt) {
		mode = COMPILE;
		do_optimize = true;
	}

	string name(filename);
	size_t i = name.rfind('/') + 1, j = name.rfind('.');
	string llPath = name.substr(i, j - i) + ".ll";
	name = name.substr(i);

	FileSource source(filename);
	Lexer lexer(&source);
	Parser parser(&lexer);
	Buffer<Parser::Output> buffer(&parser);

	Analyzer analyzer(buffer.createChild());
	IRGenerator generator(&analyzer, name);
	Optimizer optimizer(&generator, generator.modPtr.get());
	FileSink output(
		do_optimize ?
			(Source<IRFragment> *)&optimizer :
			(Source<IRFragment> *)&generator, generator.modPtr.get(),
		llPath,
		debug_mode
	);

	if (hasOpt)
		optimizer.options = opt;

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
	} catch (common::Error &e) {
		fprintf(stderr, "%s:%d:%d: error: %s\n", filename, e.pos.line, e.pos.column, e.message.c_str());
		exit(1);
	}

	if (debug_mode && mode != COMPILE) {
		parser.print_debug_tree();
		parser.print_context();
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
	IRGenerator generator(&analyzer, "repl");
	JITEngine jit(&generator, &generator); // @todo style

	source.parser = &parser;

	while (true) {
		try {
			jit.next(nullptr);
		} catch (common::Error &e) {
			fprintf(stderr, "stdin:%d:%d: error: %s\n", e.pos.line, e.pos.column, e.message.c_str());
		}
	}
}

int main(int argc, const char *argv[]) {
	opt.inl = false;
	opt.cse = false;
	opt.licm = false;
	opt.symex = false;
	opt.decom = false;

	for (int i = 1; i < argc; ++i) {
		if      (!strcmp(argv[i], "--tokenize" )) mode = TOKENIZE;
		else if (!strcmp(argv[i], "--debug"    )) debug_mode = true;
		else if (!strcmp(argv[i], "--no-sema"  )) do_sema = false;
		else if (!strcmp(argv[i], "--dry"      )) enable_output = false;
		else if (!strcmp(argv[i], "--parse"    )) mode = PARSE;
		else if (!strcmp(argv[i], "--print-ast")) mode = PRINT_AST;
		else if (!strcmp(argv[i], "--repl"     )) repl();
		else if (!strcmp(argv[i], "--compile"  )) mode = COMPILE;
		else if (!strcmp(argv[i], "--optimize" )) mode = OPTIMIZE;

		else if (!strcmp(argv[i], "--opt-inl"  )) opt.inl   = hasOpt = true;
		else if (!strcmp(argv[i], "--opt-cse"  )) opt.cse   = hasOpt = true;
		else if (!strcmp(argv[i], "--opt-licm" )) opt.licm  = hasOpt = true;
		else if (!strcmp(argv[i], "--opt-symex")) opt.symex = hasOpt = true;
		else if (!strcmp(argv[i], "--opt-decom")) opt.decom = hasOpt = true;

		else if (!strcmp(argv[i], "--optimize-compile-time")) mode = COMPILE;
		else if (!strcmp(argv[i], "--optimize-run-time"))     mode = OPTIMIZE;

		else parse(argv[i]);
	}
}
