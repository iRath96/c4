#ifndef FileSink_h
#define FileSink_h

#include "streams.h"
#include <string>


namespace llvm {
	class GlobalValue;
	class Module;
}


namespace compiler {

class IRGenerator;
struct IRFragment;
class FileSink : public streams::Stream<IRFragment, void> {
protected:
	llvm::Module *mod;
public:
	std::string outPath;
	bool print;

	FileSink(Source<IRFragment> *source, llvm::Module *mod, std::string outPath, bool print = false);
	virtual bool next(void *);
};

}

#endif /* FileSink_h */
