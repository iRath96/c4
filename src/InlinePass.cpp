#include "InlinePass.h"
#include "OptimizerPass.h"
#include "OptimizerUtils.h"
#include "DecompilerPass.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#pragma GCC diagnostic pop


using namespace std;
using namespace llvm;

void InlinePass::processCall(CallInst *call, set<Function *> &dirtyFns) {
	auto block = call->getParent();
	auto caller = call->getFunction();
	auto callee = dyn_cast<Function>(call->getCalledValue());
	if (!callee || callee->isDeclaration()) return;

	auto retBlock = block->splitBasicBlock(call, "inline-ret");
	block->getTerminator()->eraseFromParent();

	auto retPHI = PHINode::Create(call->getType(), 0);
	retPHI->insertBefore(call);

	map<Value *, Value *> vmap;

	auto arg = callee->args().begin();
	for (auto &param : call->arg_operands()) {
		vmap[arg] = param.get();
		++arg;
	}

	for (auto &cblock : callee->getBasicBlockList()) {
		auto newBB =
			&cblock == &callee->getEntryBlock() ?
				block :
				BasicBlock::Create(block->getContext(), cblock.getName().str() + "-" + callee->getName().str(), caller)
		;

		vmap[&cblock] = newBB;

		for (auto &inst : cblock.getInstList()) {
			if (auto ret = dyn_cast<ReturnInst>(&inst)) { // @todo not DRY
				auto clone = BranchInst::Create(retBlock);
				vmap[&inst] = clone;
				newBB->getInstList().push_back(clone);

				retPHI->addIncoming(ret->getReturnValue(), newBB);
			} else {
				auto clone = inst.clone();
				vmap[&inst] = clone;
				newBB->getInstList().push_back(clone);
			}
		}
	}

	vmap[retPHI] = retPHI; // so we fix operands here as well

	for (auto &v : vmap) {
		auto inst = dyn_cast<Instruction>(v.second);
		if (!inst) continue;

		for (auto &op : inst->operands()) {
			if (vmap.find(op.get()) == vmap.end())
				// no replacement available, must be a global reference
				continue;

			op.set(vmap[op.get()]);
		}
	}

	call->replaceAllUsesWith(retPHI);
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
