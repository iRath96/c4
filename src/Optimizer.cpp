#include "Optimizer.h"
#include "IRGenerator.h"

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

using namespace compiler;


namespace optimizer {

Optimizer::Optimizer(Source<IRFragment> *source, Module *module)
: Stream<IRFragment, IRFragment>(source), fpm(module), module(module) {
	fpm.add(createPromoteMemoryToRegisterPass());
	fpm.add(new OptimizerPass(this));
	fpm.add(new DecompilerPass(this));
}

bool Optimizer::next(IRFragment *result) {
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

}
