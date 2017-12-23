#include "FileSink.h"
#include "Compiler.h"

#include <stddef.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include <llvm/Support/SystemUtils.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#pragma GCC diagnostic pop

FileSink::FileSink(Source<CompilerResult> *source, llvm::Module *mod, std::string outPath, bool print)
: Stream<CompilerResult, void>(source), mod(mod), outPath(outPath), print(print) {}

bool FileSink::next(void *) {
	CompilerResult cres;
	if (this->source->next(&cres)) return true;
	else {
		std::error_code EC;
		llvm::raw_fd_ostream stream(outPath, EC, llvm::sys::fs::OpenFlags::F_Text);
		mod->print(stream, nullptr);
		if (print) mod->print(llvm::errs(), nullptr);

		return false;
	}
}
