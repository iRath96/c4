#include "InlinePass.h"
#include "OptimizerPass.h"
#include "OptimizerUtils.h"
#include "DecompilerPass.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#pragma GCC diagnostic pop


using namespace std;
using namespace llvm;


namespace optimizer {

void InlinePass::processCall(CallInst *call, set<Function *> &dirtyFns) {
	auto callBlock = call->getParent(); // the block the call occurs in
	auto caller = call->getFunction();
	auto callee = dyn_cast<Function>(call->getCalledValue());

	if (!callee || callee->isDeclaration())
		// might be a function pointer or some function we don't control
		return;

	string prefix = callee->getName().str() + ".";

	if (callee->getBasicBlockList().size() > 16)
		// too complex, not going to inline this
		return;

	vector<BasicBlock *> bbs;
	for (auto &b : callee->getBasicBlockList())
		// we copy those so that we only look at the original blocks in case of recursion
		bbs.push_back(&b);

	// placeholder target for inlined return statements
	auto exitPlaceholder = BasicBlock::Create(callBlock->getContext(), prefix + "exit", caller);

	// joins all return statements of the inlined function
	PHINode *exitPHI = nullptr;
	if (!call->getType()->isVoidTy())
		exitPHI = PHINode::Create(call->getType(), 0, prefix + "return");

	// maps values from their originals to their inlined clones
	map<Value *, Value *> vmap;

	{
		// map all call parameters to their arguments
		auto arg = callee->args().begin();
		for (auto &param : call->arg_operands()) {
			vmap[arg] = param.get();
			++arg;
		}
	}

	for (auto &cblock : bbs) {
		auto newBB = BasicBlock::Create(
			callBlock->getContext(),
			prefix + cblock->getName().str(),
			caller
		);

		vmap[cblock] = newBB;

		for (auto &inst : cblock->getInstList()) {
			// the instruction list shouldn't change while we're in this loop
			// (it can only do so in the case of infinite recursion)

			if (auto ret = dyn_cast<ReturnInst>(&inst)) { // @todo not DRY
				auto clone = BranchInst::Create(exitPlaceholder);
				vmap[&inst] = clone;
				newBB->getInstList().push_back(clone);

				if (exitPHI)
					exitPHI->addIncoming(ret->getReturnValue(), newBB);
			} else {
				auto clone = inst.clone();
				vmap[&inst] = clone;
				if (dyn_cast<AllocaInst>(clone)) {
					// when inlining functions into loops, we have to make sure that
					// 'alloca's are called only once, otherwise we'd get a stack overflow
					// (@bug this will break for 'alloca's that are intended to be called multiple times)
					auto &entry = caller->getEntryBlock();
					entry.getInstList().insert(entry.getFirstInsertionPt(), clone);
				} else
					newBB->getInstList().push_back(clone);
			}
		}
	}

	if (exitPHI)
		// this becomes the first instruction in the exit block
		exitPHI->insertAfter(call);

	// fix control flow
	auto exitBlock = callBlock->splitBasicBlock(call, "inline-ret");
	exitPlaceholder->replaceAllUsesWith(exitBlock);
	exitPlaceholder->eraseFromParent();

	// replace the branch that splitBasicBlock created and instead jump to the inlined entry
	callBlock->getTerminator()->eraseFromParent();
	callBlock->getInstList().push_back(BranchInst::Create(
		(BasicBlock *)vmap[&callee->getEntryBlock()]
	));

	if (exitPHI)
		vmap[exitPHI] = exitPHI; // we need to fix operands in this one as well

	for (auto &v : vmap) {
		if (dyn_cast<Argument>(v.first))
			// the arguments need to replacing as they are in the original scope
			continue;

		auto clone = dyn_cast<Instruction>(v.second);
		if (!clone) continue;

		for (auto &op : clone->operands())
			if (vmap.find(op.get()) != vmap.end())
				op.set(vmap[op.get()]);
			// else: must be a global variable

		if (auto phi = dyn_cast<PHINode>(clone))
			// update block references for phi nodes
			for (auto &b : phi->blocks())
				if (vmap.find(b) != vmap.end())
					b = (BasicBlock *)vmap[b];
	}

	if (exitPHI)
		// provide return value
		call->replaceAllUsesWith(vmap[exitPHI]);
	
	call->eraseFromParent();

	// this function has been modified and is therefore dirty
	dirtyFns.insert(caller);
}

bool InlinePass::runOnModule(Module &module) {
	legacy::FunctionPassManager fpm(&module);
	fpm.add(new OptimizerPass(optimizer));
	fpm.add(new DecompilerPass(optimizer));

	// find all call instructions in our module
	vector<CallInst *> calls;
	for (auto &func : module.functions())
		for (auto &block : func.getBasicBlockList())
			for (auto &inst : block.getInstList())
				if (auto call = dyn_cast<CallInst>(&inst))
					calls.push_back(call);

	// try to inline each call we've found
	set<Function *> dirtyFns;
	for (auto &call : calls)
		processCall(call, dirtyFns);

	// rerun optimizer on dirty functions as the analysis might now be more precise
	// (and we're also able to simplify some control flow)
	for (auto &dirtyFn : dirtyFns)
		fpm.run(*dirtyFn);

	return dirtyFns.size() > 0; // returns whether we modified something
}

char InlinePass::ID = 1;

}
