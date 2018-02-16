#ifndef Optimizer_h
#define Optimizer_h

#include "streams.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/IR/LegacyPassManager.h>
#pragma GCC diagnostic pop


namespace llvm {
	class Module;
}


class Compiler;
struct CompilerResult;
class Optimizer : public streams::Stream<CompilerResult, CompilerResult> {
protected:
	llvm::legacy::FunctionPassManager fpm;
	llvm::Module *module;
public:
	struct Options {
		bool inl   = true;
		bool licm  = true;
		bool cse   = true; // @todo
		bool symex = false;
		bool decom = false;
	} options;

	Optimizer(Source<CompilerResult> *source, llvm::Module *module);
	virtual bool next(CompilerResult *);
};

#endif /* Optimizer_h */
