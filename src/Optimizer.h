#ifndef Optimizer_h
#define Optimizer_h

#include "streams.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include <llvm/IR/LegacyPassManager.h>
#pragma GCC diagnostic pop

namespace llvm {
	class Module;
}

extern bool experimentalOpt;

class Compiler;
struct CompilerResult;
class Optimizer : public streams::Stream<CompilerResult, CompilerResult> {
protected:
	llvm::legacy::FunctionPassManager fpm;
public:
	Optimizer(Source<CompilerResult> *source, llvm::Module *mod);
	virtual bool next(CompilerResult *);
};

#endif /* Optimizer_h */
