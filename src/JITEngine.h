#ifndef JITEngine_h
#define JITEngine_h

#include "streams.h"


namespace llvm {
	class GlobalValue;
	class ExecutionEngine;
}


namespace compiler {

class IRGenerator;
struct IRFragment;
class JITEngine : public streams::Stream<IRFragment, void> {
protected:
	llvm::ExecutionEngine *engine;
public:
	JITEngine(Source<IRFragment> *source, IRGenerator *generator);

	virtual bool next(void *);
};

}

#endif /* JITEngine_h */
