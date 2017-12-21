#include "Optimizer.h"
#include "Compiler.h"

#include "llvm/Pass.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

struct HelloPass : public FunctionPass {
	static char ID;
	HelloPass() : FunctionPass(ID) {}

	bool runOnFunction(llvm::Function &func) override {
		errs() << "Hello: ";
		errs().write_escaped(func.getName()) << '\n';
		return false;
	}
};

char HelloPass::ID = 0;

Optimizer::Optimizer(Source<CompilerResult> *source, Module *mod)
: Stream<CompilerResult, CompilerResult>(source), fpm(mod) {
	fpm.add(createPromoteMemoryToRegisterPass());
	//fpm.add(new HelloPass());
}

bool Optimizer::next(CompilerResult *result) {
	if (this->source->next(result)) {
		for (auto &value : result->values)
			if (isa<llvm::Function>(value))
				fpm.run(*cast<llvm::Function>(value));
		return true;
	} else
		return false;
}
