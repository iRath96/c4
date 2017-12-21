#ifndef Optimizer_h
#define Optimizer_h

#include "streams.h"
#include "llvm/IR/LegacyPassManager.h"

namespace llvm {
	class Module;
}

class Compiler;
struct CompilerResult;
class Optimizer : public Stream<CompilerResult, CompilerResult> {
protected:
	llvm::legacy::FunctionPassManager fpm;
public:
	Optimizer(Source<CompilerResult> *source, llvm::Module *mod);
	virtual bool next(CompilerResult *);
};

#endif /* Optimizer_h */
