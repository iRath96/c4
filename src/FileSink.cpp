#include "FileSink.h"
#include "IRGenerator.h"

#include <stddef.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include <llvm/Support/SystemUtils.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#pragma GCC diagnostic pop


namespace compiler {

FileSink::FileSink(Source<IRFragment> *source, llvm::Module *mod, std::string outPath, bool print)
: Stream<IRFragment, void>(source), mod(mod), outPath(outPath), print(print) {}

bool FileSink::next(void *) {
	IRFragment cres;
	if (this->source->next(&cres)) return true;
	else {
		std::error_code EC;
		llvm::raw_fd_ostream stream(outPath, EC, llvm::sys::fs::OpenFlags::F_Text);
		mod->print(stream, nullptr);
		if (print) mod->print(llvm::errs(), nullptr);

		return false;
	}
}

}
