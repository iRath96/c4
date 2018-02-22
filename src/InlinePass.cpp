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
	auto block = call->getParent();
	auto caller = call->getFunction();
	auto callee = dyn_cast<Function>(call->getCalledValue());
	if (!callee || callee->isDeclaration()) return;

	if (callee->getBasicBlockList().size() > 16)
		// too complex, not going to inline this
		return;

	//bool recursion = caller == callee;

	vector<BasicBlock *> bbs;
	for (auto &b : callee->getBasicBlockList())
		// we copy those so that we only look at the original blocks in case of recursion
		bbs.push_back(&b);

	auto callBlock = block;
	auto retBlock = BasicBlock::Create(block->getContext(), "inline-ret", caller);
	block = BasicBlock::Create(block->getContext(), "inline-call", caller);

	PHINode *retPHI = nullptr;
	if (!call->getType()->isVoidTy())
		retPHI = PHINode::Create(call->getType(), 0);

	map<Value *, Value *> vmap;

	auto arg = callee->args().begin();
	for (auto &param : call->arg_operands()) {
		vmap[arg] = param.get();
		++arg;
	}

	for (auto &cblock : bbs) {
		auto newBB =
			cblock == &callee->getEntryBlock() ?
				block :
				BasicBlock::Create(block->getContext(), cblock->getName().str() + "-" + callee->getName().str(), caller)
		;

		vmap[cblock] = newBB;

		for (auto &inst : cblock->getInstList()) {
			// the instruction list shouldn't change while we're in this loop
			// (it can only do so in the case of infinite recursion)

			if (auto ret = dyn_cast<ReturnInst>(&inst)) { // @todo not DRY
				auto clone = BranchInst::Create(retBlock);
				vmap[&inst] = clone;
				newBB->getInstList().push_back(clone);

				if (retPHI)
					retPHI->addIncoming(ret->getReturnValue(), newBB);
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

	if (retPHI)
		retPHI->insertAfter(call);

	auto realRetBlock = callBlock->splitBasicBlock(call, "inline-ret");
	retBlock->replaceAllUsesWith(realRetBlock);
	retBlock->eraseFromParent();

	callBlock->getTerminator()->eraseFromParent();
	callBlock->getInstList().push_back(BranchInst::Create(block));

	vmap[retPHI] = retPHI ?
		(Value *)retPHI :
		(Value *)UndefValue::get(call->getType())
	; // so we fix operands here as well

	for (auto &v : vmap) {
		if (!v.first)
			// this is the PHI node we copied
			continue;

		if (dyn_cast<Argument>(v.first))
			continue;

		auto clone = dyn_cast<Instruction>(v.second);
		if (!clone)
			continue;

		// @todo surprise, surprise! as with apparently all of my code, this is not elegant…

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

	call->replaceAllUsesWith(vmap[retPHI]);
	call->eraseFromParent();

	dirtyFns.insert(caller);
}

bool InlinePass::runOnModule(Module &module) {
	legacy::FunctionPassManager fpm(&module);
	fpm.add(new OptimizerPass(optimizer));
	fpm.add(new DecompilerPass(optimizer));

	vector<CallInst *> calls;

	for (auto &func : module.functions())
		for (auto &block : func.getBasicBlockList())
			for (auto &inst : block.getInstList())
				if (auto call = dyn_cast<CallInst>(&inst))
					calls.push_back(call);

	set<Function *> dirtyFns;
	for (auto &call : calls)
		processCall(call, dirtyFns);

	for (auto &dirtyFn : dirtyFns)
		fpm.run(*dirtyFn);

	return true;
}

char InlinePass::ID = 1;

}

