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
class FileSink : public Stream<CompilerResult, void> {
protected:
	llvm::Module *mod;
public:
	std::string outPath;
	
	FileSink(Compiler *source, std::string outPath);

	virtual bool next(void *);
};

#endif /* FileSink_h */
