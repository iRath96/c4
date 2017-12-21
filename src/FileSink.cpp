#include "FileSink.h"
#include "Compiler.h"

#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

FileSink::FileSink(Compiler *source, std::string outPath)
: Stream<CompilerResult, void>(source), outPath(outPath), mod(source->modPtr.get()) {}

bool FileSink::next(void *) {
	CompilerResult cres;
	if (this->source->next(&cres)) return true;
	else {
		std::error_code EC;
		llvm::raw_fd_ostream stream(outPath, EC, llvm::sys::fs::OpenFlags::F_Text);
		mod->print(stream, nullptr);

		return false;
	}
}
