#include "IRGenerator.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wmacro-redefined"
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/DebugLoc.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/Support/Signals.h>
#include <llvm/Support/SystemUtils.h>
#include <llvm/Support/PrettyStackTrace.h>
#pragma GCC diagnostic pop


extern bool debug_mode;


namespace compiler {

using namespace ast;

llvm::Type *IRGenerator::createType(const Type *type) {
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
	} else if (dynamic_cast<const VoidType *>(type)) {
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

llvm::Value *IRGenerator::getValue(Expression &expr, bool load, bool logicalNeeded) {
	// @todo very ugly
	bool prevSL = shouldLoad;
	bool prevLN = logical.needed;
	auto prevTBB = logical.tBB;
	auto prevFBB = logical.fBB;

	shouldLoad = load;
	logical.needed = logicalNeeded;

	inspect(expr);

	if (logical.needed) {
		if (value->getType() != builder.getInt1Ty())
			value = builder.CreateICmpNE(value, matchType(builder.getInt1(0), value->getType()));
		builder.CreateCondBr(value, logical.tBB, logical.fBB);
		logical.needed = false;
	}

	logical.tBB = prevTBB;
	logical.fBB = prevFBB;
	logical.prevLN = logical.needed;

	logical.needed = prevLN;
	shouldLoad = prevSL;

	return value;
}

llvm::Value *IRGenerator::matchType(llvm::Value *value, llvm::Type *type) {
	if (type->isVoidTy()) return nullptr; // discard value ("return (void)...;")
	if (type == value->getType()) return value;
	if (value->getType()->isPointerTy())
		return builder.CreatePointerCast(value, type);
	if (type->isPointerTy())
		return builder.CreateIntToPtr(value, type);
	return builder.CreateIntCast(value, type, true, "cast");
}

IRGenerator::IRGenerator(Source<Ptr<ast::External>> *source, std::string moduleName, bool emitDebug)
:
	Stream<Ptr<External>, IRFragment>(source),
	mod(new llvm::Module(moduleName, ctx)),
	builder(ctx), allocaBuilder(ctx), diBuilder(*mod),
	dataLayout(mod),
	emitDebug(emitDebug)
{
	modPtr.reset(mod);

	if (emitDebug) {
		diFile = diBuilder.createFile(moduleName, ".");
		diStack.push(
			diBuilder.createCompileUnit(
				llvm::dwarf::DW_LANG_C,
				diFile,
				"acom",
				0,
				"",
				0
			)
		);

		mod->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
	}
}

void IRGenerator::inspect(ast::Node &node) {
	if (emitDebug) {
		auto loc = llvm::DebugLoc::get(node.pos.line, node.pos.column, diStack.top());
		builder.SetCurrentDebugLocation(loc);
		allocaBuilder.SetCurrentDebugLocation(loc);
	}

	node.accept(*this);
}

bool IRGenerator::next(IRFragment *result) {
	Ptr<External> ext;
	if (this->source->next(&ext)) {
		cres.values.clear();
		cres.shouldExecute = dynamic_cast<REPLStatement *>(ext.get()) ? true : false;

		inspect(*ext);
		*result = cres;

		return true;
	} else {
		if (emitDebug)
			diBuilder.finalize();
		if (debug_mode)
			llvm::verifyModule(*mod, &llvm::errs());
		return false;
	}
}

void IRGenerator::visit(REPLStatement &node) {
	std::vector<llvm::Type *> argTypes;
	auto fnType = llvm::FunctionType::get(builder.getVoidTy(), argTypes, false);
	func = llvm::Function::Create(
		fnType, llvm::GlobalValue::ExternalLinkage, "anonymous", mod
	);

	auto entry = llvm::BasicBlock::Create(ctx, "entry", func, 0);
	builder.SetInsertPoint(entry);
	allocaBuilder.SetInsertPoint(entry);

	inspect(*node.statement);

	if (debug_mode)
		llvm::verifyFunction(*func, &llvm::errs());

	cres.values.push_back(func);
}

void IRGenerator::createLabels(const PtrVector<Label> &labels) {
	for (auto &lab : labels)
		if (auto il = dynamic_cast<IdentifierLabel *>(lab.get())) {
			auto block = llvm::BasicBlock::Create(ctx, il->id, func, 0);
			builder.CreateBr(block);
			builder.SetInsertPoint(block);

			this->labels[il->id] = block;
			for (auto &ref : labelRefs[il->id]) ref->setSuccessor(0, block);
		} else
			throw AnalyzerError("only identifier labels supported", lab->pos); // @todo IRGeneratorError
}

void IRGenerator::visit(CompoundStatement &node) {
	createLabels(node.labels);
	for (auto &item : node.items)
		inspect(*item);
}

void IRGenerator::visit(Declaration &node) { declaration(node, false); }
void IRGenerator::declaration(Declaration &node, bool isGlobal) {
	for (const auto &decl : node.declarators) {
		auto dr = (DeclarationRef *)decl.annotation.get();
		if (values.find(dr) == values.end()) {
			if (dynamic_cast<FunctionType *>(dr->type.get())) {
				auto type = (llvm::FunctionType *)createType(dr->type.get());
				auto func = llvm::Function::Create(
					type, llvm::GlobalValue::ExternalLinkage, decl.name, mod
				);

				values[dr] = func;
			} else if (isGlobal) {
				auto type = createType(dr->type.get());

				bool external = node.isExternal;
				auto global = new llvm::GlobalVariable(
					*mod, type, false,
					external ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::CommonLinkage,
					external ? nullptr : llvm::Constant::getNullValue(type),
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

void IRGenerator::visit(GlobalVariable &node) {
	declaration(node.declaration, true);
}

void IRGenerator::visit(Function &node) {
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
		auto value = allocaBuilder.CreateAlloca(argIt->getType(), nullptr, "param." + param.declarator.name);
		builder.CreateStore(argIt, value);
		values[(DeclarationRef *)param.annotation.get()] = value;

		argIt++;
	}

	for (auto &d : node.declarations)
		inspect(d);

	// debug information

	if (emitDebug) {
		llvm::SmallVector<llvm::Metadata *, 8> eltTypes;
		llvm::DIType *voidType = diBuilder.createBasicType("void", 0, llvm::dwarf::DW_ATE_signed);

		eltTypes.push_back(voidType); // return type

		for (unsigned i = 0; i < func->arg_size(); ++i)
			eltTypes.push_back(voidType);

		auto dist = diBuilder.createSubroutineType(diBuilder.getOrCreateTypeArray(eltTypes));
		auto diSub = diBuilder.createFunction(
			diFile,
			func->getName(),
			llvm::StringRef(),
			diFile,
			node.pos.line,
			dist,
			false,
			true,
			0
		);

		func->setSubprogram(diSub);

		diStack.push(diSub);
	}

	inspect(node.body);

	if (emitDebug)
		diStack.pop();

	// always create return
	if (builder.GetInsertBlock()->getTerminator() == nullptr) {
		auto retType = builder.getCurrentFunctionReturnType();
		if (retType->isVoidTy()) builder.CreateRetVoid();
		else builder.CreateRet(llvm::Constant::getNullValue(retType));
	}

	if (debug_mode)
		llvm::verifyFunction(*func, &llvm::errs()); // @todo also verifyFunction in Optimizer
}

void IRGenerator::visit(IdentifierExpression &node) {
	auto dr = (DeclarationRef *)node.annotation.get();
	value = values[dr];
	if (shouldLoad && !dr->type->isFunction()) // cannot load functions
		value = builder.CreateLoad(value, "rval");
}

void IRGenerator::visit(Constant &node) {
	if (node.isChar) value = builder.getInt8(node.value);
	else value = builder.getInt32(node.value);
}

void IRGenerator::visit(StringLiteral &node) {
	value = builder.CreateGlobalStringPtr(node.value);
}

void IRGenerator::visit(UnaryExpression &node) {
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
		value = performAdd(value, builder.getInt8(1), "preinc");
		builder.CreateStore(value, var);
	}; break;
	case P::MINUSMINUS: {
		auto var = getValue(*node.operand, false);
		value = builder.CreateLoad(value, "load");
		value = performSub(value, builder.getInt8(1), "predec");
		builder.CreateStore(value, var);
	}; break;

	// others
	case P::MINUS: value = builder.CreateNeg(getValue(*node.operand)); break;
	case P::PLUS: value = getValue(*node.operand); break;
	case P::BIT_NOT: value = builder.CreateNot(getValue(*node.operand)); break;
	case P::LOG_NOT: createLogicalNot(node); break;

	default: throw AnalyzerError("operation not supported", node.pos); // @todo IRGeneratorError
	}
}

llvm::PHINode *IRGenerator::createLogicalPHI(llvm::BasicBlock *&post) {
	if (logical.needed) {
		post = builder.GetInsertBlock();
		return nullptr;
	} else {
		auto pre = builder.GetInsertBlock();
		post = llvm::BasicBlock::Create(ctx, "logical.join", func, 0);

		logical.tBB = llvm::BasicBlock::Create(ctx, "true", func, 0);
		logical.fBB = llvm::BasicBlock::Create(ctx, "false", func, 0);

		builder.SetInsertPoint(logical.tBB);
		builder.CreateBr(post);

		builder.SetInsertPoint(logical.fBB);
		builder.CreateBr(post);

		builder.SetInsertPoint(post);

		auto phi = builder.CreatePHI(builder.getInt32Ty(), 2);
		phi->addIncoming(builder.getInt32(0), logical.fBB);
		phi->addIncoming(builder.getInt32(1), logical.tBB);

		builder.SetInsertPoint(pre);

		return phi;
	}
}

void IRGenerator::createLogicalAnd(BinaryExpression &node) {
	llvm::BasicBlock *post;
	auto phi = createLogicalPHI(post);

	auto prevTBB = logical.tBB;
	auto myTBB = logical.tBB = llvm::BasicBlock::Create(ctx, "and.cont", func, 0);
	getValue(*node.lhs, true, true);

	builder.SetInsertPoint(myTBB);
	logical.tBB = prevTBB;
	getValue(*node.rhs, true, true);

	builder.SetInsertPoint(post);

	value = phi;
	logical.needed = false;
}

void IRGenerator::createLogicalOr(BinaryExpression &node) {
	llvm::BasicBlock *post;
	auto phi = createLogicalPHI(post);

	auto prevFBB = logical.fBB;
	auto myFBB = logical.fBB = llvm::BasicBlock::Create(ctx, "or.cont", func, 0);
	getValue(*node.lhs, true, true);

	builder.SetInsertPoint(myFBB);
	logical.fBB = prevFBB;
	getValue(*node.rhs, true, true);

	builder.SetInsertPoint(post);

	value = phi;
	logical.needed = false;
}

void IRGenerator::createLogicalNot(UnaryExpression &node) {
	llvm::BasicBlock *post;
	auto phi = createLogicalPHI(post);

	auto prevFBB = logical.fBB;
	auto prevTBB = logical.tBB;

	logical.fBB = prevTBB;
	logical.tBB = prevFBB;

	getValue(*node.operand, true, true);

	logical.tBB = prevTBB;
	logical.fBB = prevFBB;

	builder.SetInsertPoint(post);

	value = phi;
	logical.needed = false;
}

llvm::Value *IRGenerator::performAdd(llvm::Value *lhs, llvm::Value *rhs, std::string name) {
	bool lptr = lhs->getType()->isPointerTy();
	bool rptr = rhs->getType()->isPointerTy();

	if (lptr || rptr) return builder.CreateGEP(lptr ? lhs : rhs, lptr ? rhs : lhs, name);
	return builder.CreateAdd(lhs, matchType(rhs, lhs->getType()), name);
}

llvm::Value *IRGenerator::performSub(llvm::Value *lhs, llvm::Value *rhs, std::string name) {
	bool lptr = lhs->getType()->isPointerTy();
	bool rptr = rhs->getType()->isPointerTy();
	
	if (lptr && rptr) return builder.CreatePtrDiff(lhs, rhs, name);
	else if (lptr) return builder.CreateGEP(lhs, builder.CreateNeg(rhs, "inv"), name);
	else return builder.CreateSub(lhs, matchType(rhs, lhs->getType()), name);
}

void IRGenerator::visit(BinaryExpression &node) {
	using Op = lexer::Token::Punctuator;
	using Prec = lexer::Token::Precedence;

	auto prec = lexer::Token::precedence(node.op);
	if (prec != Prec::ASSIGNMENT) assert(shouldLoad == true);

	if (prec == Prec::LOGICAL_AND) return createLogicalAnd(node);
	if (prec == Prec::LOGICAL_OR) return createLogicalOr(node);

	auto lhs = getValue(*node.lhs, prec != Prec::ASSIGNMENT);
	auto rhs = getValue(*node.rhs);
	auto type = lhs->getType(); // @todo createType from annotation

	auto mlhs = lhs;
	auto mrhs = rhs;

	if (prec == Prec::ASSIGNMENT) {
		type = type->getPointerElementType();
		mrhs = matchType(rhs, type);
	} else if (auto lit = llvm::dyn_cast<llvm::IntegerType>(lhs->getType()))
		if (auto rit = llvm::dyn_cast<llvm::IntegerType>(rhs->getType())) {
			if (rit->getBitWidth() > lit->getBitWidth()) {
				type = rhs->getType();
				mlhs = matchType(lhs, type);
			} else {
				mrhs = matchType(rhs, type);
			}
		}

	switch (node.op) {
	// assignments @todo
	case Op::ASSIGN: {
		builder.CreateStore(mrhs, lhs);
		value = lhs;
		if (shouldLoad) value = builder.CreateLoad(value, "deref"); // @todo not DRY
	}; break;

	// relational, @todo not DRY
	case Op::CMP_LTE:  value = builder.CreateICmpSLE(mlhs, mrhs, "cmp"); break;
	case Op::AB_OPEN:  value = builder.CreateICmpSLT(mlhs, mrhs, "cmp"); break;
	case Op::CMP_GTE:  value = builder.CreateICmpSGE(mlhs, mrhs, "cmp"); break;
	case Op::AB_CLOSE: value = builder.CreateICmpSGT(mlhs, mrhs, "cmp"); break;
	case Op::CMP_EQ:   value = builder.CreateICmpEQ (mlhs, mrhs, "cmp"); break;
	case Op::CMP_NEQ:  value = builder.CreateICmpNE (mlhs, mrhs, "cmp"); break;

	// arithmetic
	case Op::PLUS:  value = performAdd(mlhs, mrhs); break;
	case Op::MINUS: value = performSub(mlhs, mrhs); break;

	// multiplicative
	case Op::ASTERISK: value = builder.CreateMul(mlhs, mrhs); break;
	case Op::SLASH: value = builder.CreateSDiv(mlhs, mrhs); break;
	case Op::MODULO: value = builder.CreateSRem(mlhs, mrhs); break;
	default: throw AnalyzerError("operation not supported", node.pos); // @todo IRGeneratorError
	}
}

void IRGenerator::visit(ConditionalExpression &node) {
	auto ifTrue = llvm::BasicBlock::Create(ctx, "ternary.true", func, 0);
	auto ifFalse = llvm::BasicBlock::Create(ctx, "ternary.false", func, 0);
	auto end = llvm::BasicBlock::Create(ctx, "ternary.end", func, 0);

	logical.tBB = ifTrue;
	logical.fBB = ifFalse;

	getValue(*node.condition, true, true);

	builder.SetInsertPoint(ifTrue);
	auto vTrue = getValue(*node.whenTrue);
	builder.CreateBr(end);

	builder.SetInsertPoint(ifFalse);
	auto vFalse = getValue(*node.whenFalse);
	builder.CreateBr(end);

	builder.SetInsertPoint(end);

	auto phiType = vTrue->getType();
	if (phiType->isVoidTy()) {
		value = nullptr;
		return;
	}

	auto phi = builder.CreatePHI(phiType, 2);
	phi->addIncoming(matchType(vFalse, phiType), logical.fBB);
	phi->addIncoming(matchType(vTrue, phiType), logical.tBB);
	value = phi;
}

void IRGenerator::visit(ExpressionList &node) {
	for (auto &child : node.children) {
		bool isLast = &child == &node.children.back();
		getValue(*child, isLast ? shouldLoad : true, isLast ? logical.needed : false);
	}
	
	logical.needed = logical.prevLN;
}

void IRGenerator::visit(CallExpression &node) {
	auto fn = getValue(*node.function, false);
	auto fnPtrType = (llvm::PointerType *)fn->getType();

	if (!llvm::isa<llvm::FunctionType>(fnPtrType->getElementType())) {
		// @todo not elegant
		fn = builder.CreateLoad(value, "deref");
		fnPtrType = (llvm::PointerType *)fn->getType();
	}

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

void IRGenerator::visit(CastExpression &node) {
	auto type = createType(((DeclarationRef *)node.annotation.get())->type.get());
	value = matchType(getValue(*node.expression), type);
}

void IRGenerator::visit(SubscriptExpression &node) {
	value = performAdd(getValue(*node.base), getValue(node.subscript));
	if (shouldLoad) value = builder.CreateLoad(value, "subs");
}

void IRGenerator::visit(MemberExpression &node) {
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

void IRGenerator::visit(PostExpression &node) {
	using P = lexer::Token::Punctuator;

	switch (node.op) {
	// post inc-/decrement
	case P::PLUSPLUS: { // @todo not DRY
		auto var = getValue(*node.base, false);
		value = builder.CreateLoad(value, "load");
		auto v = performAdd(value, builder.getInt8(1), "postinc");
		builder.CreateStore(v, var);
	}; break;
	case P::MINUSMINUS: {
		auto var = getValue(*node.base, false);
		value = builder.CreateLoad(value, "load");
		auto v = performSub(value, builder.getInt8(1), "postdec");
		builder.CreateStore(v, var);
	}; break;
	default: throw AnalyzerError("operation not supported", node.pos); // @todo IRGeneratorError
	}
}

void IRGenerator::visit(ExpressionStatement &node) {
	createLabels(node.labels);

	shouldLoad = true;
	inspect(node.expressions);
}

void IRGenerator::createSizeof(ast::Node &node) {
	auto &type = ((TypePair *)node.annotation.get())->type;
	size_t size = type->getSizeOverride();
	if (!size) size = dataLayout.getTypeAllocSize(createType(type.get()));
	value = builder.getInt32((uint32_t)size);
}

void IRGenerator::visit(SizeofExpressionTypeName &node) {
	createSizeof(node.type);
}

void IRGenerator::visit(SizeofExpressionUnary &node) {
	createSizeof(*node.expression);
}

void IRGenerator::visit(IterationStatement &node) {
	auto prevLoop = loop;
	createLabels(node.labels);

	loop.header = llvm::BasicBlock::Create(ctx, "while.header", func, 0);
	loop.body = llvm::BasicBlock::Create(ctx, "while.body", func, 0);
	loop.end = llvm::BasicBlock::Create(ctx, "while.end", func, 0);

	logical.tBB = loop.body;
	logical.fBB = loop.end;

	builder.CreateBr(loop.header);
	builder.SetInsertPoint(loop.header);

	getValue(node.condition, true, true);

	builder.SetInsertPoint(loop.body);
	inspect(*node.body);
	builder.CreateBr(loop.header);
	builder.SetInsertPoint(loop.end);

	loop = prevLoop;
}

void IRGenerator::visit(SelectionStatement &node) {
	createLabels(node.labels);

	auto ifTrue = llvm::BasicBlock::Create(ctx, "if.true", func, 0);
	auto end = llvm::BasicBlock::Create(ctx, "if.end", func, 0);
	auto ifFalse = node.whenFalse.get() ? llvm::BasicBlock::Create(ctx, "if.false", func, 0) : end;

	logical.tBB = ifTrue;
	logical.fBB = ifFalse;

	getValue(node.condition, true, true);

	builder.SetInsertPoint(ifTrue);
	inspect(*node.whenTrue);
	builder.CreateBr(end);

	if (node.whenFalse.get()) {
		builder.SetInsertPoint(ifFalse);
		inspect(*node.whenFalse);
		builder.CreateBr(end);
	}

	builder.SetInsertPoint(end);
}

void IRGenerator::createDeadBlock() {
	// @todo don't even bother inserting here!
	auto deadBlock = llvm::BasicBlock::Create(ctx, "ghost", func, 0);
	builder.SetInsertPoint(deadBlock);
}

void IRGenerator::visit(ReturnStatement &node) {
	createLabels(node.labels);

	builder.CreateRet(matchType(getValue(node.expressions), func->getReturnType()));
	createDeadBlock();
}

void IRGenerator::visit(GotoStatement &node) {
	auto target = labels[node.target];
	if (!target) target = builder.GetInsertBlock(); // will be patched later
	labelRefs[node.target].push_back(builder.CreateBr(target));
	createDeadBlock();
}

void IRGenerator::visit(ContinueStatement &node) {
	createLabels(node.labels);
	builder.CreateBr(node.keyword == lexer::Token::Keyword::BREAK ? loop.end : loop.header);
	createDeadBlock();
}

}
