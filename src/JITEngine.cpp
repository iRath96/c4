#include "JITEngine.h"
#include "Compiler.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wmacro-redefined"
#include <llvm/Support/SystemUtils.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
//#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/GlobalValue.h>
#pragma GCC diagnostic pop

#include <iostream>
#include <memory>

JITEngine::JITEngine(Source<CompilerResult> *source, Compiler *compiler) : Stream<CompilerResult, void>(source) {
	LLVMInitializeNativeTarget();
	LLVMInitializeNativeAsmPrinter();

	std::string errStr;
	engine = llvm::EngineBuilder(std::move(compiler->modPtr))
		.setErrorStr(&errStr)
		//.setUseMCJIT(true)
		.setMCJITMemoryManager(llvm::make_unique<llvm::SectionMemoryManager>())
		.create();

	if (!engine) {
		std::cerr << errStr << std::endl;
		exit(1);
	}
}

bool JITEngine::next(void *) {
	CompilerResult cres;
	if (this->source->next(&cres)) {
		for (auto &global : cres.values) {
			global->print(llvm::errs(), true);
			std::cout << std::endl;

			//auto ptr = engine->getPointerToGlobal(global);
			if (cres.shouldExecute) {
				std::cout << "execute!" << std::endl;

				auto func = (llvm::Function *)global;
				std::vector<llvm::GenericValue> args;
				engine->runFunction(func, args);

				//auto ptr = engine->getPointerToFunction(func);
				//((void (*)())ptr)();
			}
		}

		return true;
	} else return false;
}
