#ifndef JITEngine_h
#define JITEngine_h

#include "streams.h"


namespace llvm {
	class GlobalValue;
	class ExecutionEngine;
}


class Compiler;
struct CompilerResult;
class JITEngine : public streams::Stream<CompilerResult, void> {
protected:
	llvm::ExecutionEngine *engine;
public:
	JITEngine(Source<CompilerResult> *source, Compiler *compiler);

	virtual bool next(void *);
};

#endif /* JITEngine_h */
