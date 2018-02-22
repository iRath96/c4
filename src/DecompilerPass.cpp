#include "DecompilerPass.h"

#include "OptimizerPass.h"
#include "OptimizerUtils.h"
#include "Beautifier.h"


using namespace llvm;
using namespace std;


namespace optimizer {

void DecompilerPass::getAnalysisUsage(AnalysisUsage &info) const {
	info.addRequired<OptimizerPass>();
	info.setPreservesAll();
}

string legalizeName(const string &str) {
	std::string out = "";
	for (auto &c : str) {
		if (c >= 'a' && c <= 'z') out += c;
		else if (c >= 'A' && c <= 'Z') out += c;
		else if (c >= '0' && c <= '9' && !out.empty()) out += c;
		else out += "_";
	}

	return out;
}

string DecompilerPass::resolveName(Value *value) {
	if (names.find(value) != names.end())
		return names[value];

	string test = legalizeName(value->getName().str());

	while (true) {
		if (!test.empty() && namesReverse.find(test) == namesReverse.end()) {
			names[value] = test;
			namesReverse[test] = value;
			return test;
		}

		if (nameCounter < 26)
			test = 'a' + (nameCounter++);
		else
			test = "_" + std::to_string(nameCounter++);
	}
}

bool DecompilerPass::isLoop(BasicBlock *header, BasicBlock *body) {
	auto &opt = getAnalysis<OptimizerPass>();

	if (header == body) return true;
	if (!opt.blocks[body].isDominatedBy(header)) return false;

	auto br = dyn_cast<BranchInst>(body->getTerminator());
	if (!br) return false;

	if (isLoop(header, br->getSuccessor(0))) return true;
	if (br->isConditional() && isLoop(header, br->getSuccessor(1))) return true;

	return false;
}

void DecompilerPass::bindBlock(BasicBlock &block, ast::CompoundStatement &compound, set<BasicBlock *> &join) {
	auto &opt = getAnalysis<OptimizerPass>();
	auto index = compound.items.size();

	// decompile instructions
	decompileBlock(block, compound);

	// decompile terminator
	auto term = block.getTerminator();
	if (auto branch = dyn_cast<BranchInst>(term)) {
		if (branch->isConditional()) {
			auto ss = make_shared<ast::SelectionStatement>();
			ss->condition.children.push_back(resolve(branch->getCondition()));
			
			set<BasicBlock *> subJoin;
			set<BasicBlock *> alreadyBound;
			bool owned[2];

			for (int i = 0; i < 2; ++i) {
				auto succ = branch->getSuccessor(i);
				auto otherSucc = branch->getSuccessor(1 - i);

				owned[i] = opt.blocks[succ].isDominatedBy(&block) && &block != succ; // @todo not DRY!

				bool isMergeBlock = false;
				for (auto &e : opt.blocks[succ].edges)
					if (opt.blocks[e.first].isDominatedBy(otherSucc)) {
						// @todo if neither successor is a merge block, we could freely choose one
						// that we deem more complicated, as the other will obviously terminate
						isMergeBlock = true;
						break;
					}

				auto c = make_shared<ast::CompoundStatement>();
				if (owned[i] && !isMergeBlock) {
					//cout << "cond " << resolveName(succ) << " into " << resolveName(&block) << endl;
					bindBlock(*succ, *c, subJoin);
					alreadyBound.insert(succ);
				} else {
					// @todo not quite DRY with unconditional

					auto gs = make_shared<ast::GotoStatement>();
					gs->target = resolveName(succ);
					c->items.push_back(gs); // we put this in a compound statement so fixGotos is happy

					if (owned[i])
						// will be bound later
						subJoin.insert(succ);
					else
						// will not be bound
						join.insert(succ);
				}

				(i == 0 ? ss->whenTrue : ss->whenFalse) = c;
			}

			compound.items.push_back(ss);

			// both owned false => we dominate no blocks

			for (auto &b : alreadyBound)
				subJoin.erase(b);

			//cout << "sub-join " << resolveName(&block) << endl;
			for (auto &b : subJoin) {
				if (opt.blocks[b].isDominatedBy(&block) && &block != b) {
					// we need to bind this
					//cout << "* binding " << resolveName(b) << " into " << resolveName(&block) << endl;
					bindBlock(*b, compound, join);
				} else
					// not our job
					join.insert(b);
			}
		} else {
			auto succ = branch->getSuccessor(0);
			if (opt.blocks[succ].isDominatedBy(&block) && &block != succ) {
				// cout << "uncond " << resolveName(succ) << " into " << resolveName(&block) << endl;
				bindBlock(*succ, compound, join);

				// every block we dominate is also dominated by succ,
				// succ will bind all dominated blocks for us
			} else {
				// goto statement
				auto gs = make_shared<ast::GotoStatement>();
				gs->target = resolveName(succ);
				compound.items.push_back(gs);

				join.insert(succ);

				// no block is dominated by us
			}
		}
	} else if (auto ret = dyn_cast<ReturnInst>(term)) {
		// return statement
		auto rs = make_shared<ast::ReturnStatement>();
		rs->expressions.children.push_back(resolve(ret->getReturnValue()));
		compound.items.push_back(rs);

		// no block is dominated by us
	} else {
		cerr << "unsupported terminator" << endl;
		assert(false);
	}

	discard_if(join, [&](llvm::BasicBlock *b) {
		return opt.blocks[b].isDominatedBy(&block);
	});

	assert(compound.items.size() > index);

	auto e = compound.items[index];
	dynamic_cast<ast::Statement *>(e.get())->labels.push_back(
		make_shared<ast::IdentifierLabel>(resolveName(&block))
	);
}

bool DecompilerPass::fixGotos(ast::Statement *body, set<string> &refs, ast::IdentifierLabel *follow) {
	auto compound = dynamic_cast<ast::CompoundStatement *>(body);
	if (!compound)
		return false;

	assert(!compound->items.empty());

	for (size_t i = 0; i < compound->items.size(); ++i) {
		auto &stmt = compound->items[i];
		ast::IdentifierLabel *il = nullptr;

		if (i == compound->items.size() - 1) {
			il = follow;
		} else {
			auto &follow = compound->items[i+1];
			if (auto s = dynamic_cast<ast::Statement *>(follow.get()))
				if (!s->labels.empty())
					il = dynamic_cast<ast::IdentifierLabel *>(s->labels[0].get());
		}

		if (auto ss = dynamic_cast<ast::SelectionStatement *>(stmt.get())) {
			bool tEmpty = fixGotos(ss->whenTrue.get(), refs, il);
			bool fEmpty = fixGotos(ss->whenFalse.get(), refs, il);

			// try to simplify if statement:

			if (tEmpty) {
				swap(tEmpty, fEmpty);
				swap(ss->whenTrue, ss->whenFalse);

				negateExpression(ss->condition);
			}

			if (fEmpty)
				ss->whenFalse.reset();

			// @todo could remove if statement if tEmpty

			unwrapCompoundStatement(ss->whenTrue);
			unwrapCompoundStatement(ss->whenFalse);
		} else if (auto is = dynamic_cast<ast::IterationStatement *>(stmt.get())) {
			fixGotos(is->body.get(), refs, il);
			// hmpf, not supported yet though
		}
	}

	if (auto gs = dynamic_cast<ast::GotoStatement *>(compound->items.back().get())) {
		if (follow && gs->target == follow->id)
			compound->items.pop_back();
		else
			refs.insert(gs->target);
	}

	return compound->items.empty();
}

void DecompilerPass::fixLabels(ast::Statement *body, const std::set<std::string> &refs) {
	if (!body)
		// might be empty else part of if statement
		return;

	if (!body->labels.empty())
		if (auto il = dynamic_cast<ast::IdentifierLabel *>(body->labels[0].get()))
			if (refs.find(il->id) == refs.end())
				body->labels.clear();

	// @todo not DRY with fixGotos
	if (auto ss = dynamic_cast<ast::SelectionStatement *>(body)) {
		fixLabels(ss->whenTrue.get(), refs);
		fixLabels(ss->whenFalse.get(), refs);
	} else if (auto is = dynamic_cast<ast::IterationStatement *>(body)) {
		fixLabels(is->body.get(), refs);
	}

	auto compound = dynamic_cast<ast::CompoundStatement *>(body);
	if (!compound) return;

	for (auto &item : compound->items)
		if (auto stmt = dynamic_cast<ast::Statement *>(item.get()))
			fixLabels(stmt, refs);
}

void DecompilerPass::unwrapCompoundStatement(ast::Ptr<ast::Statement> &stmt) {
	if (auto c = dynamic_cast<ast::CompoundStatement *>(stmt.get()))
		if (c->items.size() == 1)
			stmt = static_pointer_cast<ast::Statement>(c->items.front()); // isn't C++ just lovely?
}

void DecompilerPass::negateExpression(ast::ExpressionList &expr) {
	using Punct = lexer::Token::Punctuator;

	auto last = expr.children.back();
	expr.children.pop_back();

	if (auto ue = dynamic_cast<ast::UnaryExpression *>(last.get()))
		if (ue->op == Punct::LOG_NOT) {
			expr.children.push_back(ue->operand);
			return;
		}

	if (auto be = dynamic_cast<ast::BinaryExpression *>(last.get())) {
		// we can modify this, as it has only one use
		switch (be->op) {
			case Punct::CMP_EQ: be->op = Punct::CMP_NEQ; break;
			case Punct::CMP_NEQ: be->op = Punct::CMP_EQ; break;
			case Punct::CMP_LTE: be->op = Punct::AB_CLOSE; break;
			case Punct::AB_CLOSE: be->op = Punct::CMP_LTE; break;
			case Punct::CMP_GTE: be->op = Punct::AB_OPEN; break;
			case Punct::AB_OPEN: be->op = Punct::CMP_GTE; break;
			default: goto deny;
		}

		expr.children.push_back(last);
		return;

		deny: {}
	}

	auto neg = make_shared<ast::UnaryExpression>();
	neg->operand = last;
	neg->op = Punct::LOG_NOT;
	expr.children.push_back(neg);
}

void DecompilerPass::decompileType(
	Type *type,
	ast::PtrVector<ast::TypeSpecifier> &specifiers,
	ast::PtrVector<ast::DeclaratorModifier> &modifiers
) {
	std::string name = "unsupported";

	if (type->isVoidTy())
	 	name = "void";
	else if (auto it = dyn_cast<IntegerType>(type)) {
		switch (it->getBitWidth()) {
			case 8: name = "char"; break;
			case 32: name = "int"; break;
		}
	} else if (auto pt = dyn_cast<PointerType>(type)) {
		modifiers.push_back(make_shared<ast::DeclaratorPointer>());
		decompileType(pt->getElementType(), specifiers, modifiers);
	}

	specifiers.push_back(make_shared<ast::NamedTypeSpecifier>(name));
}

bool DecompilerPass::runOnFunction(llvm::Function &func) {
	if (!optimizer->options.decom)
		return false;

	ignoreInst.clear();
	phiRefs.clear();
	vmap.clear();
	names.clear();
	namesReverse.clear();
	nameCounter = 0;
	
	auto &opt = getAnalysis<OptimizerPass>();

	auto topology = opt.buildTopology();
	for (auto &block : topology) {
		//auto firstDom = findFirstDominator(*block);
		for (auto &phi : block->phis()) {
			// create declaration for this
			ast::Declarator d;
			d.name = resolveName(&phi);

			auto s = make_shared<ast::NamedTypeSpecifier>();
			s->id = "int";

			auto decl = make_shared<ast::Declaration>();
			decl->declarators.push_back(d);
			decl->specifiers.push_back(s);

			vmap[&phi] = make_shared<ast::IdentifierExpression>(d.name);

			for (auto &src : phi.blocks())
				phiRefs[src].push_back(&phi);
		}
	}

	streams::VectorSource<ast::Ptr<ast::External>> buffer;
	utils::Beautifier beauty(&buffer);
	beauty.lispMode = false;

	ast::Declarator decl;
	decl.name = legalizeName(func.getName().str());

	auto dpl = make_shared<ast::DeclaratorParameterList>();
	for (auto &arg : func.args()) {
		ast::ParameterDeclaration pd;
		decompileType(arg.getType(), pd.specifiers, pd.declarator.modifiers);
		pd.declarator.name = arg.getName().str();
		dpl->parameters.push_back(pd);
	} // @todo variadic

	decl.modifiers.push_back(dpl);

	auto fn = make_shared<ast::Function>();
	decompileType(func.getReturnType(), fn->declaration.specifiers, decl.modifiers);
	fn->declaration.declarators.push_back(decl);
	buffer.data.push_back(fn); // @todo this might actually be a memory issue?

	set<BasicBlock *> join;
	bindBlock(func.getEntryBlock(), fn->body, join);

	set<string> refs;
	fixGotos(&fn->body, refs);
	fixLabels(&fn->body, refs);

	beauty.drain();

	for (auto &b : join) cout << b->getName().str() << " not bound" << endl;
	assert(join.empty());

	return false;
}

llvm::BasicBlock *DecompilerPass::findFirstDominator(BasicBlock &block) {
	auto &opt = getAnalysis<OptimizerPass>();
	auto &doms = opt.blocks[&block].dominators;

	// find the closest strict dominator by finding the dominator which itself
	// dominates the most blocks (excluding our own block, of course)
	return *max_element(doms.begin(), doms.end(), [&](BasicBlock *a, BasicBlock *b) {
		// exclude our own block (sort as minimum)
		if (a == &block) return true;
		if (b == &block) return false;

		// sort by dominator count
		return opt.blocks[a].dominators.size() < opt.blocks[b].dominators.size();
	});
}

ast::Ptr<ast::Expression> DecompilerPass::resolve(Value *value) {
	if (!value)
		// e.g. `return void`
		return nullptr;

	if (auto cint = dyn_cast<ConstantInt>(value)) {
		int v = (int)*cint->getValue().getRawData();
		return make_shared<ast::Constant>(to_string(v), v, false);
	}

	if (vmap.find(value) != vmap.end()) return vmap[value];

	// might be a global or an argument
	return make_shared<ast::IdentifierExpression>(resolveName(value));
}

void DecompilerPass::resolvePHIRefs(BasicBlock &block, ast::CompoundStatement &compound) {
	for (auto &phiRef : phiRefs[&block]) {
		auto phiVal = phiRef->getIncomingValueForBlock(&block);

		auto ass = make_shared<ast::BinaryExpression>();
		ass->lhs = resolve(phiRef);
		ass->rhs = resolve(phiVal);
		ass->op = lexer::Token::Punctuator::ASSIGN;

		auto es = make_shared<ast::ExpressionStatement>();
		es->expressions.children.push_back(ass);

		compound.items.push_back(es);
	}
}

void DecompilerPass::decompileBlock(BasicBlock &block, ast::CompoundStatement &compound) {
	auto &opt = getAnalysis<OptimizerPass>();

	for (auto &inst : block.getInstList()) {
		if (inst.isTerminator()) {
			// needs to happen _before_ terminator
			resolvePHIRefs(block, compound);
			return;
		}

		if (ignoreInst[&inst])
			continue;

		if (vmap.find(&inst) != vmap.end())
			// translated this already, must be a PHI node
			continue;

		ast::Ptr<ast::Expression> expr;
		ast::Ptr<ast::BlockItem> bi;

		if (auto icmp = dyn_cast<ICmpInst>(&inst)) {
			auto be = make_shared<ast::BinaryExpression>();
			be->lhs = resolve(icmp->getOperand(0));
			be->rhs = resolve(icmp->getOperand(1));

			using Punct = lexer::Token::Punctuator;
			switch (icmp->getPredicate()) {
				case CmpInst::ICMP_EQ: be->op = Punct::CMP_EQ; break;
				case CmpInst::ICMP_NE: be->op = Punct::CMP_NEQ; break;
				case CmpInst::ICMP_SLT: be->op = Punct::AB_OPEN; break;
				case CmpInst::ICMP_SGT: be->op = Punct::AB_CLOSE; break;
				case CmpInst::ICMP_SLE: be->op = Punct::CMP_LTE; break;
				case CmpInst::ICMP_SGE: be->op = Punct::CMP_GTE; break;
				default: be->op = Punct::NOT_A_PUNCTUATOR;
			}

			expr = be;
		} else if (auto bin = dyn_cast<BinaryOperator>(&inst)) {
			auto be = make_shared<ast::BinaryExpression>();
			be->lhs = resolve(bin->getOperand(0));
			be->rhs = resolve(bin->getOperand(1));

			using Punct = lexer::Token::Punctuator;
			switch (bin->getOpcode()) {
				case Instruction::Sub: be->op = Punct::MINUS; break;
				case Instruction::Add: be->op = Punct::PLUS; break;
				case Instruction::Mul: be->op = Punct::ASTERISK; break;
				case Instruction::SDiv: be->op = Punct::SLASH; break;
				default: be->op = Punct::NOT_A_PUNCTUATOR;
			}

			expr = be;
		} else if (auto call = dyn_cast<CallInst>(&inst)) {
			auto ce = make_shared<ast::CallExpression>();
			ce->function = resolve(call->getCalledFunction());
			for (auto &arg : call->arg_operands())
				ce->arguments.push_back(resolve(arg.get()));
			expr = ce;
		} else {
			expr = make_shared<ast::StringLiteral>("unsupported");
		}

		bool needsStore = opt.hasSideEffect(&inst) || inst.getNumUses() > 1;
		if (!needsStore && inst.getNumUses() == 1) {
			auto &use = *inst.uses().begin();
			auto i = dyn_cast<Instruction>(use.getUser());

			if (i && i->getParent() != &block)
				needsStore = true;
		}

		if (needsStore) { // @todo not DRY
			assert(!bi.get());

			ast::Declarator d;

			if (inst.getNumUses() > 0) {
				d.name = resolveName(&inst);

				ast::Ptr<ast::Expression> ie = make_shared<ast::IdentifierExpression>(d.name);
				expr = make_shared<ast::BinaryExpression>(
					ie,
					expr,
					lexer::Token::Punctuator::ASSIGN
				);
			}
			
			auto es = make_shared<ast::ExpressionStatement>();
			es->expressions.children.push_back(expr);
			bi = es;

			auto s = make_shared<ast::NamedTypeSpecifier>();
			s->id = "int";

			auto decl = make_shared<ast::Declaration>();
			decl->declarators.push_back(d);
			decl->specifiers.push_back(s);
			// @todo push back declaration

			compound.items.push_back(bi);
			vmap[&inst] = make_shared<ast::IdentifierExpression>(d.name);
		} else {
			vmap[&inst] = expr;
		}
	}
}

char DecompilerPass::ID = 3;

}

