#ifndef FileSink_h
#define FileSink_h

#include "streams.h"
#include <string>

namespace llvm {
	class GlobalValue;
	class Module;
}

class Compiler;
struct CompilerResult;
class FileSink : public streams::Stream<CompilerResult, void> {
protected:
	llvm::Module *mod;
public:
	std::string outPath;
	bool print;

	FileSink(Source<CompilerResult> *source, llvm::Module *mod, std::string outPath, bool print = false);
	virtual bool next(void *);
};

#endif /* FileSink_h */
