#include "DecompilerPass.h"

#include "OptimizerPass.h"
#include "OptimizerUtils.h"
#include "Beautifier.h"


using namespace llvm;
using namespace std;


void DecompilerPass::getAnalysisUsage(AnalysisUsage &info) const {
	info.addRequired<OptimizerPass>();
	info.setPreservesAll();
}

std::string legalizeName(const std::string &str) {
	std::string out = "";
	for (auto &c : str) {
		if (c >= 'a' && c <= 'z') out += c;
		else if (c >= 'A' && c <= 'Z') out += c;
		else if (c >= '0' && c <= '9' && !out.empty()) out += c;
		else out += "_";
	}

	return out;
}

bool DecompilerPass::runOnFunction(llvm::Function &func) {
	auto &opt = getAnalysis<OptimizerPass>();

	streams::VectorSource<ast::Ptr<ast::External>> buffer;
	Beautifier beauty(&buffer);

	auto fn = make_shared<ast::Function>();
	buffer.data.push_back(fn); // @todo this might actually be a memory issue?

	auto topology = opt.buildTopology();

	for (auto &block : topology) {
		// create compound statements for each block

		auto label = make_shared<ast::IdentifierLabel>();
		label->id = legalizeName(block->getName().str());
		labels[block] = label->id;

		auto cs = make_shared<ast::CompoundStatement>();
		cs->labels.push_back(label);

		auto firstDom = findFirstDominator(block);
		for (auto &phi : block->phis()) {
			// create declaration for this
			ast::Declarator d;
			d.name = legalizeName(phi.getName().str());

			auto s = make_shared<ast::NamedTypeSpecifier>();
			s->id = "int";

			auto decl = make_shared<ast::Declaration>();
			decl->declarators.push_back(d);
			decl->specifiers.push_back(s);

			bmap[firstDom]->items.insert(bmap[firstDom]->items.begin(), decl);
			vmap[&phi] = make_shared<ast::IdentifierExpression>(d.name);

			for (auto &src : phi.blocks())
				phiRefs[src].push_back(&phi);
		}

		//bmap[firstDom]->items.push_back(cs);
		bmap[block] = cs;
	}

	for (auto &block : topology)
		decompileBlock(block);

	fn->body = *bmap[&func.getEntryBlock()]; // @todo not elegant

	beauty.drain();
	return false;
}

llvm::BasicBlock *DecompilerPass::findFirstDominator(BasicBlock *block) {
	auto &opt = getAnalysis<OptimizerPass>();
	auto &doms = opt.blocks[block].dominators;

	// find the closest strict dominator by finding the dominator which itself
	// dominates the most blocks (excluding our own block, of course)
	return *max_element(doms.begin(), doms.end(), [&](BasicBlock *a, BasicBlock *b) {
		// exclude our own block (sort as minimum)
		if (a == block) return true;
		if (b == block) return false;

		// sort by dominator count
		return opt.blocks[a].dominators.size() < opt.blocks[b].dominators.size();
	});
}

ast::Ptr<ast::Expression> DecompilerPass::resolve(Value *value) {
	if (auto cint = dyn_cast<ConstantInt>(value)) {
		int v = (int)*cint->getValue().getRawData();
		return make_shared<ast::Constant>(to_string(v), v, false);
	}

	if (vmap.find(value) != vmap.end()) return vmap[value];

	// might be a global or an argument
	return make_shared<ast::IdentifierExpression>(legalizeName(value->getName().str()));
}

void DecompilerPass::resolvePHIRefs(BasicBlock *block) {
	for (auto &phiRef : phiRefs[block]) {
		auto phiVal = phiRef->getIncomingValueForBlock(block);

		auto ass = make_shared<ast::BinaryExpression>();
		ass->lhs = resolve(phiRef);
		ass->rhs = resolve(phiVal);
		ass->op = lexer::Token::Punctuator::ASSIGN;

		auto es = make_shared<ast::ExpressionStatement>();
		es->expressions.children.push_back(ass);

		bmap[block]->items.push_back(es);
	}
}

void DecompilerPass::decompileBlock(BasicBlock *block) {
	auto &opt = getAnalysis<OptimizerPass>();

	for (auto &inst : block->getInstList()) {
		if (inst.isTerminator())
			// needs to happen _before_ terminator
			resolvePHIRefs(block);

		if (ignoreInst[&inst])
			continue;

		if (vmap.find(&inst) != vmap.end())
			// translated this already, must be a PHI node
			continue;

		ast::Ptr<ast::Expression> expr;
		ast::Ptr<ast::BlockItem> bi;

		if (auto ret = dyn_cast<ReturnInst>(&inst)) {
			auto rs = make_shared<ast::ReturnStatement>();
			rs->expressions.children.push_back(resolve(ret->getReturnValue()));
			bi = rs;
		} else if (auto icmp = dyn_cast<ICmpInst>(&inst)) {
			auto be = make_shared<ast::BinaryExpression>();
			be->lhs = resolve(icmp->getOperand(0));
			be->rhs = resolve(icmp->getOperand(1));
			be->op = lexer::Token::Punctuator::QMARK;
			expr = be;
		} else if (auto call = dyn_cast<CallInst>(&inst)) {
			auto ce = make_shared<ast::CallExpression>();
			ce->function = resolve(call->getFunction());
			for (auto &arg : call->arg_operands())
				ce->arguments.push_back(resolve(arg.get()));
			expr = ce;
		} else if (auto branch = dyn_cast<BranchInst>(&inst)) {
			if (branch->isConditional()) {
				auto ss = make_shared<ast::SelectionStatement>();
				ss->condition.children.push_back(resolve(branch->getCondition()));
				ss->when_true = bmap[branch->getSuccessor(0)];

				auto trueTerm = branch->getSuccessor(0)->getTerminator();
				auto bTT = dyn_cast<BranchInst>(trueTerm);
				if (bTT && !bTT->isConditional() && bTT->getSuccessor(0) == branch->getSuccessor(1)) {
					// no else needed!
					ignoreInst[trueTerm] = true;
					bmap[branch->getSuccessor(1)] = bmap[block];
				} else {
					ss->when_false = bmap[branch->getSuccessor(1)];
				}

				bi = ss;
			} else {
				auto gs = make_shared<ast::GotoStatement>();
				gs->target = labels[branch->getSuccessor(0)];
				bi = gs;
			}
		} else {
			expr = make_shared<ast::StringLiteral>("unsupported");
		}

		bool needsStore = opt.hasSideEffect(&inst) || inst.getNumUses() > 1;
		if (needsStore) { // @todo not DRY
			if (!bi.get()) {
				auto es = make_shared<ast::ExpressionStatement>();
				es->expressions.children.push_back(expr);
				bi = es;
			}

			ast::Declarator d;
			d.name = legalizeName(inst.getName().str());

			auto s = make_shared<ast::NamedTypeSpecifier>();
			s->id = "int";

			auto decl = make_shared<ast::Declaration>();
			decl->declarators.push_back(d);
			decl->specifiers.push_back(s);

			bmap[block]->items.push_back(bi);
			vmap[&inst] = make_shared<ast::IdentifierExpression>(d.name);
		} else {
			vmap[&inst] = expr;
		}
	}
}

char DecompilerPass::ID = 3;
