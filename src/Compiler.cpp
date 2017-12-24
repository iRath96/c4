#include "Compiler.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Signals.h>
#include <llvm/Support/SystemUtils.h>
#include <llvm/Support/PrettyStackTrace.h>
#pragma GCC diagnostic pop

llvm::Type *Compiler::createType(const Type *type) {
	auto it = types.find(type);
	if (it != types.end()) return it->second;

	llvm::Type *result = NULL;

	if (auto at = dynamic_cast<const ArithmeticType *>(type)) {
		switch (at->size) {
		case ArithmeticType::CHAR: result = builder.getInt8Ty(); break;
		default: result = builder.getInt32Ty(); break;
		}
	} else if (auto pt = dynamic_cast<const PointerType *>(type)) {
		result = pt->isVoidPointer() ? builder.getInt8PtrTy() : llvm::PointerType::get(createType(pt->base.get()), 0);
	} else if (auto fn = dynamic_cast<const FunctionType *>(type)) {
		std::vector<llvm::Type *> argTypes;
		for (const auto &param : fn->parameters) argTypes.push_back(createType(param.get()));
		result = llvm::FunctionType::get(createType(fn->returnType.get()), argTypes, fn->isVariadic);
	} else if (auto v = dynamic_cast<const VoidType *>(type)) {
		result = builder.getVoidTy();
	} else if (auto ct = dynamic_cast<const ComposedType *>(type)) {
		if (ct->kind == lexer::Token::Keyword::UNION)
			throw new AnalyzerError("unions are not yet supported", ct->pos);

		auto str = llvm::StructType::create(ctx, ct->name);
		types[type] = str; // set it directly to break recursion

		std::vector<llvm::Type *> members;
		for (const auto &mem : ct->members) members.push_back(createType(mem.type.get()));
		str->setBody(members);

		result = str;
	}

	return types[type] = result;
}

llvm::Value *Compiler::getValue(Expression &expr, bool load) {
	bool prevSL = shouldLoad;
	shouldLoad = load;
	inspect(expr);
	shouldLoad = prevSL;
	return value;
}

llvm::Value *Compiler::matchType(llvm::Value *value, llvm::Type *type) {
	if (type == value->getType()) return value;
	if (value->getType()->isPointerTy())
		return builder.CreatePointerCast(value, type);
	return builder.CreateIntCast(value, type, true, "cast");
}

Compiler::Compiler(Source<Ptr<ast::External>> *source, std::string moduleName)
: Stream<Ptr<External>, CompilerResult>(source), builder(ctx), allocaBuilder(ctx) {
	mod = new llvm::Module(moduleName, ctx);
	modPtr.reset(mod);
}

bool Compiler::next(CompilerResult *result) {
	Ptr<External> ext;
	if (this->source->next(&ext)) {
		cres.values.clear();
		cres.shouldExecute = dynamic_cast<REPLStatement *>(ext.get()) ? true : false;

		inspect(ext);
		*result = cres;

		return true;
	} else {
		llvm::verifyModule(*mod);
		return false;
	}
}

void Compiler::visit(REPLStatement &node) {
	std::vector<llvm::Type *> argTypes;
	auto fnType = llvm::FunctionType::get(builder.getVoidTy(), argTypes, false);
	func = llvm::Function::Create(
		fnType, llvm::GlobalValue::ExternalLinkage, "anonymous", mod
	);

	auto entry = llvm::BasicBlock::Create(ctx, "entry", func, 0);
	builder.SetInsertPoint(entry);
	allocaBuilder.SetInsertPoint(entry);

	inspect(node.statement);

	llvm::verifyFunction(*func);

	cres.values.push_back(func);
}

void Compiler::createLabels(const PtrVector<Label> &labels) {
	for (auto &lab : labels)
		if (auto il = dynamic_cast<IdentifierLabel *>(lab.get())) {
			auto block = llvm::BasicBlock::Create(ctx, il->id, func, 0);
			builder.CreateBr(block);
			builder.SetInsertPoint(block);

			this->labels[il->id] = block;
			for (auto &ref : labelRefs[il->id]) ref->setSuccessor(0, block);
		} else
			throw AnalyzerError("only identifier labels supported", lab->pos); // @todo CompilerError
}

void Compiler::visit(CompoundStatement &node) {
	createLabels(node.labels);
	for (auto &item : node.items) inspect(item);
}

void Compiler::visit(Declaration &node) { declaration(node, false); }
void Compiler::declaration(Declaration &node, bool isGlobal) {
	for (const auto &decl : node.declarators) {
		auto dr = (DeclarationRef *)decl.annotation.get();
		if (values.find(dr) == values.end()) {
			if (auto fn = dynamic_cast<FunctionType *>(dr->type.get())) {
				auto type = (llvm::FunctionType *)createType(dr->type.get());
				auto func = llvm::Function::Create(
					type, llvm::GlobalValue::ExternalLinkage, decl.name, mod
				);

				values[dr] = func;
			} else if (isGlobal) {
				auto type = createType(dr->type.get());

				auto global = new llvm::GlobalVariable(
					*mod, type, false,
					llvm::GlobalValue::CommonLinkage,
					llvm::Constant::getNullValue(type),
					decl.name
				);

				values[dr] = global;
				cres.values.push_back(global);
			} else {
				auto type = createType(dr->type.get());

				allocaBuilder.SetInsertPoint(allocaBuilder.GetInsertBlock(), allocaBuilder.GetInsertBlock()->begin());
				auto value = allocaBuilder.CreateAlloca(type, nullptr, decl.name);

				values[dr] = value;
			}
		}

		if (decl.initializer.get()) { // @todo what about global variables?
			auto type = values[dr]->getType()->getPointerElementType();
			builder.CreateStore(matchType(getValue(*decl.initializer), type), values[dr]);
		}
	}
}

void Compiler::visit(GlobalVariable &node) {
	declaration(node.declaration, true);
}

void Compiler::visit(Function &node) {
	declaration(node.declaration, true);

	auto &decl = node.declaration.declarators.front();
	auto dr = (DeclarationRef *)decl.annotation.get();

	func = (llvm::Function *)values[dr];
	cres.values.push_back(func);

	auto entry = llvm::BasicBlock::Create(ctx, "entry", func, 0);
	builder.SetInsertPoint(entry);
	allocaBuilder.SetInsertPoint(entry);

	auto argIt = func->arg_begin();
	auto plist = dynamic_cast<DeclaratorParameterList *>(decl.modifiers.back().get());
	for (auto &param : plist->parameters) {
		argIt->setName(param.declarator.name);

		allocaBuilder.SetInsertPoint(allocaBuilder.GetInsertBlock(), allocaBuilder.GetInsertBlock()->begin());
		auto value = allocaBuilder.CreateAlloca(argIt->getType(), nullptr, "param_" + param.declarator.name);
		builder.CreateStore(argIt, value);
		values[(DeclarationRef *)param.annotation.get()] = value;

		argIt++;
	}

	for (auto &d : node.declarations) visit(d);

	visit(node.body);

	// always create return
	if (builder.GetInsertBlock()->getTerminator() == nullptr) {
		auto retType = builder.getCurrentFunctionReturnType();
		if (retType->isVoidTy()) builder.CreateRetVoid();
		else builder.CreateRet(llvm::Constant::getNullValue(retType));
	}

	llvm::verifyFunction(*func);
}

void Compiler::visit(IdentifierExpression &node) {
	auto dr = (DeclarationRef *)node.annotation.get();
	value = values[dr];
	if (shouldLoad) value = builder.CreateLoad(value, "rval");
}

void Compiler::visit(Constant &node) {
	if (node.isChar) value = builder.getInt8(node.value);
	else value = builder.getInt32(node.value);
}

void Compiler::visit(StringLiteral &node) {
	value = builder.CreateGlobalStringPtr(node.value);
}

void Compiler::visit(UnaryExpression &node) {
	using P = lexer::Token::Punctuator;
	switch (node.op) {
	// pointer operations
	case P::BIT_AND: {
		value = getValue(*node.operand, false);
	}; break;
	case P::ASTERISK: {
		value = getValue(*node.operand);
		if (shouldLoad) value = builder.CreateLoad(value, "deref");
	}; break;

	// pre inc-/decrement
	case P::PLUSPLUS: {
		auto var = getValue(*node.operand, false);
		value = builder.CreateLoad(value, "load");
		value = builder.CreateAdd(value, matchType(builder.getInt8(1), value->getType()), "pre-inc");
		builder.CreateStore(value, var);
	}; break;
	case P::MINUSMINUS: {
		auto var = getValue(*node.operand, false);
		value = builder.CreateLoad(value, "load");
		value = builder.CreateSub(value, matchType(builder.getInt8(1), value->getType()), "pre-dec");
		builder.CreateStore(value, var);
	}; break;

	// others
	case P::MINUS: value = builder.CreateNeg(getValue(*node.operand)); break;
	case P::PLUS: value = getValue(*node.operand); break;
	case P::BIT_NOT: value = builder.CreateNot(getValue(*node.operand)); break;
	case P::LOG_NOT:
		value = getValue(*node.operand);
		value = builder.CreateICmpEQ(value, matchType(builder.getInt32(0), value->getType()));
		break;

	default: throw AnalyzerError("operation not supported", node.pos); // @todo CompilerError
	}
}

void Compiler::createLogicalAnd(BinaryExpression &node) {
	auto lhs = getValue(*node.lhs);

	auto pre = builder.GetInsertBlock();
	auto c = llvm::BasicBlock::Create(ctx, "and-continue", func, 0);
	auto end = llvm::BasicBlock::Create(ctx, "and-end", func, 0);

	builder.CreateCondBr(lhs, c, end);
	builder.SetInsertPoint(c);
	auto rhs = getValue(*node.rhs);
	builder.CreateBr(end);

	builder.SetInsertPoint(end);

	auto phi = builder.CreatePHI(lhs->getType(), 2);
	phi->addIncoming(lhs, pre);
	phi->addIncoming(rhs, c);
	value = phi;
}

void Compiler::createLogicalOr(BinaryExpression &node) {
	auto lhs = getValue(*node.lhs);

	auto pre = builder.GetInsertBlock();
	auto c = llvm::BasicBlock::Create(ctx, "or-continue", func, 0);
	auto end = llvm::BasicBlock::Create(ctx, "or-end", func, 0);

	builder.CreateCondBr(lhs, end, c);
	builder.SetInsertPoint(c);
	auto rhs = getValue(*node.rhs);
	builder.CreateBr(end);

	builder.SetInsertPoint(end);

	auto phi = builder.CreatePHI(lhs->getType(), 2);
	phi->addIncoming(lhs, pre);
	phi->addIncoming(rhs, c);
	value = phi;
}

void Compiler::visit(BinaryExpression &node) {
	using Op = lexer::Token::Punctuator;
	using Prec = lexer::Token::Precedence;

	auto prec = lexer::Token::precedence(node.op);
	if (prec != Prec::ASSIGNMENT) assert(shouldLoad == true);

	if (prec == Prec::LOGICAL_AND) return createLogicalAnd(node);
	if (prec == Prec::LOGICAL_OR) return createLogicalOr(node);

	auto lhs = getValue(*node.lhs, prec != Prec::ASSIGNMENT);
	auto rhs = getValue(*node.rhs);
	auto type = lhs->getType(); // @todo createType from annotation

	if (prec == Prec::ASSIGNMENT) type = type->getPointerElementType();

	switch (node.op) {
	// assignments @todo
	case Op::ASSIGN: {
		builder.CreateStore(matchType(rhs, type), lhs);
		value = lhs;
		if (shouldLoad) value = builder.CreateLoad(value, "deref"); // @todo not DRY
	}; break;

	// relational, @todo not DRY
	case Op::CMP_LTE: value = builder.CreateICmpSLE(lhs, matchType(rhs, type), "cmp"); break;
	case Op::AB_OPEN: value = builder.CreateICmpSLT(lhs, matchType(rhs, type), "cmp"); break;
	case Op::CMP_GTE: value = builder.CreateICmpSGE(lhs, matchType(rhs, type), "cmp"); break;
	case Op::AB_CLOSE: value = builder.CreateICmpSGT(lhs, matchType(rhs, type), "cmp"); break;
	case Op::CMP_EQ: value = builder.CreateICmpEQ(lhs, matchType(rhs, type), "cmp"); break;
	case Op::CMP_NEQ: value = builder.CreateICmpNE(lhs, matchType(rhs, type), "cmp"); break;

	// arithmetic
	case Op::PLUS: {
		bool lptr = lhs->getType()->isPointerTy();
		bool rptr = rhs->getType()->isPointerTy();
		if (lptr || rptr) value = builder.CreateGEP(lptr ? lhs : rhs, lptr ? rhs : lhs, "gep");
		else value = builder.CreateAdd(lhs, rhs, "add");
	}; break;
	case Op::MINUS: {
		bool lptr = lhs->getType()->isPointerTy();
		if (lptr) value = builder.CreateGEP(lhs, builder.CreateNeg(rhs, "inv"), "gep");
		else value = builder.CreateSub(lhs, rhs);
	}; break;

	// multiplicative
	case Op::ASTERISK: value = builder.CreateMul(lhs, rhs); break;
	case Op::SLASH: value = builder.CreateSDiv(lhs, rhs); break;
	case Op::MODULO: value = builder.CreateSRem(lhs, rhs); break;
	default: throw AnalyzerError("operation not supported", node.pos); // @todo CompilerError
	}
}

void Compiler::visit(ConditionalExpression &) {
	// @todo @minor
}

void Compiler::visit(ExpressionList &node) {
	for (auto &child : node.children) inspect(child);
}

void Compiler::visit(CallExpression &node) {
	auto fn = getValue(*node.function, false);
	auto fnPtrType = (llvm::PointerType *)fn->getType();
	auto fnType = (llvm::FunctionType *)fnPtrType->getElementType();
	std::vector<llvm::Value *> args;

	unsigned int i = 0;
	for (const auto &arg : node.arguments) {
		value = getValue(*arg);
		if (i < fnType->getNumParams()) // function might be variadic
			value = matchType(value, fnType->getParamType(i++));
		args.push_back(value);
	}

	value = builder.CreateCall(fn, args);
}

void Compiler::visit(SubscriptExpression &node) {
	auto lhs = getValue(*node.base), rhs = getValue(node.subscript);
	bool lptr = lhs->getType()->isPointerTy();
	value = builder.CreateGEP(lptr ? lhs : rhs, lptr ? rhs : lhs, "gep");
	if (shouldLoad) value = builder.CreateLoad(value, "subs");
}

void Compiler::visit(MemberExpression &node) {
	auto ct = ((TypePair *)node.base->annotation.get())->type;
	if (node.dereference) ct = ct->dereference(node.pos); // @todo integrate into getMember
	auto member = ct->getMember(node.id, node.pos);

	std::vector<llvm::Value *> indices;
	indices.push_back(builder.getInt32(0));
	indices.push_back(builder.getInt32(member.index));

	value = getValue(*node.base, node.dereference);
	value = builder.CreateInBoundsGEP(value, indices);

	if (shouldLoad) value = builder.CreateLoad(value);
}

void Compiler::visit(PostExpression &node) {
	using P = lexer::Token::Punctuator;

	switch (node.op) {
	// post inc-/decrement
	case P::PLUSPLUS: { // @todo not DRY
		auto var = getValue(*node.base, false);
		value = builder.CreateLoad(value, "load");
		auto v = builder.CreateAdd(value, matchType(builder.getInt8(1), value->getType()), "post-inc");
		builder.CreateStore(v, var);
	}; break;
	case P::MINUSMINUS: {
		auto var = getValue(*node.base, false);
		value = builder.CreateLoad(value, "load");
		auto v = builder.CreateSub(value, matchType(builder.getInt8(1), value->getType()), "post-dec");
		builder.CreateStore(v, var);
	}; break;
	default: throw AnalyzerError("operation not supported", node.pos); // @todo CompilerError
	}
}

void Compiler::visit(ExpressionStatement &node) {
	createLabels(node.labels);
	visit(node.expressions);
}

void Compiler::visit(SizeofExpressionTypeName &node) {
	auto &type = ((TypePair *)node.annotation.get())->type;
	value = builder.getInt32((uint32_t)type->getSize(lexer::TextPosition())); // @todo
}

void Compiler::visit(SizeofExpressionUnary &node) {
	auto &type = ((TypePair *)node.annotation.get())->type;
	value = builder.getInt32((uint32_t)type->getSize(lexer::TextPosition()));
}

llvm::Value *Compiler::testZero(llvm::Value *v) {
	if (v->getType() == builder.getInt1Ty()) return v;
	return builder.CreateICmpNE(v, matchType(builder.getInt32(0), v->getType()), "cond");
}

void Compiler::visit(IterationStatement &node) {
	auto prevLoop = loop;

	createLabels(node.labels);

	loop.header = llvm::BasicBlock::Create(ctx, "while-header", func, 0);
	loop.body = llvm::BasicBlock::Create(ctx, "while-body", func, 0);
	loop.end = llvm::BasicBlock::Create(ctx, "while-end", func, 0);

	builder.CreateBr(loop.header);
	builder.SetInsertPoint(loop.header);
	builder.CreateCondBr(testZero(getValue(node.condition)), loop.body, loop.end);
	builder.SetInsertPoint(loop.body);
	inspect(node.body);
	builder.CreateBr(loop.header);
	builder.SetInsertPoint(loop.end);

	loop = prevLoop;
}

void Compiler::visit(SelectionStatement &node) {
	createLabels(node.labels);

	auto header = llvm::BasicBlock::Create(ctx, "if-header", func, 0); // only exists for readability
	auto ifTrue = llvm::BasicBlock::Create(ctx, "if-true", func, 0);
	auto end = llvm::BasicBlock::Create(ctx, "if-end", func, 0);
	auto ifFalse = node.when_false.get() ? llvm::BasicBlock::Create(ctx, "if-false", func, 0) : end;

	builder.CreateBr(header);
	builder.SetInsertPoint(header);
	builder.CreateCondBr(testZero(getValue(node.condition)), ifTrue, ifFalse); // @todo @important makeCF

	builder.SetInsertPoint(ifTrue);
	inspect(node.when_true);
	builder.CreateBr(end);

	if (node.when_false.get()) {
		builder.SetInsertPoint(ifFalse);
		inspect(node.when_false);
		builder.CreateBr(end);
	}

	builder.SetInsertPoint(end);
}

void Compiler::createDeadBlock() {
	// @todo don't even bother inserting here!
	auto deadBlock = llvm::BasicBlock::Create(ctx, "dead-block", func, 0);
	builder.SetInsertPoint(deadBlock);
}

void Compiler::visit(ReturnStatement &node) {
	createLabels(node.labels);

	builder.CreateRet(matchType(getValue(node.expressions), func->getReturnType()));
	createDeadBlock();
}

void Compiler::visit(GotoStatement &node) {
	auto target = labels[node.target];
	if (!target) target = builder.GetInsertBlock(); // will be patched later
	labelRefs[node.target].push_back(builder.CreateBr(target));
	createDeadBlock();
}

void Compiler::visit(ContinueStatement &node) {
	createLabels(node.labels);
	builder.CreateBr(node.keyword == lexer::Token::Keyword::BREAK ? loop.end : loop.header);
	createDeadBlock();
}
