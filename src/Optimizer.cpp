#include "Optimizer.h"

#include "InlinePass.h"
#include "OptimizerPass.h"
#include "DecompilerPass.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/Pass.h>
#include <llvm/Transforms/Scalar.h>
#pragma GCC diagnostic pop

#include <iostream>
#include <map>
#include <set>


using namespace std;
using namespace llvm;


struct CompilerResult { // @todo @fixme @important not DRY
	Compiler *compiler;
	vector<llvm::GlobalValue *> values;
	bool shouldExecute;

	CompilerResult(Compiler *compiler = nullptr) : compiler(compiler) {}
};

Optimizer::Optimizer(Source<CompilerResult> *source, Module *module)
: Stream<CompilerResult, CompilerResult>(source), fpm(module), module(module) {
	fpm.add(createPromoteMemoryToRegisterPass());
	fpm.add(new OptimizerPass(this));
	fpm.add(new DecompilerPass(this));
}

bool Optimizer::next(CompilerResult *result) {
	if (this->source->next(result)) {
		for (auto &value : result->values)
			if (isa<llvm::Function>(value)) fpm.run(*cast<llvm::Function>(value));
		return true;
	} else {
		if (options.inl) {
			InlinePass ip(this);
			ip.runOnModule(*module);
		}

		return false;
	}
}
