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

namespace compiler {
	class IRGenerator;
	struct IRFragment;
}


namespace optimizer {

class Optimizer : public streams::Stream<compiler::IRFragment, compiler::IRFragment> {
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

	Optimizer(Source<compiler::IRFragment> *source, llvm::Module *module);
	virtual bool next(compiler::IRFragment *);
};

}

#endif /* Optimizer_h */
