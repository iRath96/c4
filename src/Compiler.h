#ifndef Compiler_h
#define Compiler_h

#include "AST.h"
#include "Analyzer.h"

#include <iostream>
#include <map>
#include <vector>

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/IR/Verifier.h"              /* verifyFunction, verifyModule */
#include "llvm/Support/Signals.h"          /* Nice stacktrace output */
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

using namespace ast;

class Compiler : public Visitor, public Stream<Ptr<External>, void> {
protected:
	void inspect(Node &node) {
		node.accept(*this);
	}

	template<typename T>
	void inspect(ast::Ptr<T> &node) {
		if (node.get()) node->accept(*this);
	}

	llvm::LLVMContext ctx;
	llvm::Module mod;
	llvm::IRBuilder<> builder, allocaBuilder;

	bool shouldLoad;
	llvm::Value *value;
	llvm::Function *func;

	std::map<const DeclarationRef *, llvm::Value *> values;
	std::map<const Type *, llvm::Type *> types;

	llvm::Type *createType(const Type *type) {
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
		} else if (auto ct = dynamic_cast<const ComposedType *>(type)) { // @todo unions (and anonymous!)
			auto str = llvm::StructType::create(ctx, ct->name);

			std::vector<llvm::Type *> members;
			for (const auto &mem : ct->members) members.push_back(createType(mem.type.get()));
			str->setBody(members);

			result = str;
		}

		return types[type] = result;
	}

	llvm::Value *getValue(Expression &expr, bool load = true) {
		bool prevSL = shouldLoad;
		shouldLoad = load;
		inspect(expr);
		shouldLoad = prevSL;
		return value;
	}

	llvm::Value *matchType(llvm::Value *value, llvm::Type *type) {
		if (type == value->getType()) return value;
		return builder.CreateIntCast(value, type, true, "cast");
	}

public:
	std::string outPath;

	Compiler(std::string outPath, Source<Ptr<ast::External>> *source)
	: Stream<Ptr<External>, void>(source), mod("main.c", ctx), builder(ctx), allocaBuilder(ctx), outPath(outPath) {}

	virtual bool next(void *) {
		ast::Ptr<ast::External> result;
		if (this->source->next(&result)) {
			inspect(result);
			mod.print(llvm::errs(), nullptr);
			return true;
		} else {
			llvm::verifyModule(mod);

			std::error_code EC;
			llvm::raw_fd_ostream stream(outPath, EC, llvm::sys::fs::OpenFlags::F_Text);
			mod.print(stream, nullptr);

			return false;
		}
	}

protected:
	friend struct Node;

	virtual void visit(CompoundStatement &node) {
		// @todo labels
		for (auto &item : node.items) inspect(item);
	}

	void visit(Declaration &node) { visit(node, false); }
	void visit(Declaration &node, bool isGlobal) {
		for (const auto &decl : node.declarators) {
			auto dr = (DeclarationRef *)decl.annotation.get();
			if (values.find(dr) == values.end()) {
				if (auto fn = dynamic_cast<FunctionType *>(dr->type.get())) {
					auto type = (llvm::FunctionType *)createType(dr->type.get());
					auto func = llvm::Function::Create(
						type, llvm::GlobalValue::ExternalLinkage, decl.name, &mod
					);

					values[dr] = func;
				} else if (isGlobal) {
					auto type = createType(dr->type.get());

					auto var = new llvm::GlobalVariable(
						mod, type, false,
						llvm::GlobalValue::CommonLinkage,
						llvm::Constant::getNullValue(type),
						decl.name
					);

					values[dr] = var;
				} else {
					auto type = createType(dr->type.get());

					allocaBuilder.SetInsertPoint(allocaBuilder.GetInsertBlock(), allocaBuilder.GetInsertBlock()->begin());
					auto value = allocaBuilder.CreateAlloca(type, nullptr, decl.name);

					values[dr] = value;
				}
			}

			if (decl.initializer.get()) {
				auto type = values[dr]->getType()->getPointerElementType();
				builder.CreateStore(matchType(getValue(*decl.initializer), type), values[dr]);
			}
		}
	}

	virtual void visit(GlobalVariable &node) {
		visit(node.declaration, true);
	}

	virtual void visit(Function &node) {
		visit(node.declaration, true);

		auto &decl = node.declaration.declarators.front();
		auto dr = (DeclarationRef *)decl.annotation.get();

		func = (llvm::Function *)values[dr];

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

#pragma mark - Expressions

	virtual void visit(IdentifierExpression &node) {
		auto dr = (DeclarationRef *)node.annotation.get();
		value = values[dr];
		if (shouldLoad) value = builder.CreateLoad(value, "rval");
		return;
	}

	virtual void visit(Constant &node) {
		if (node.isChar) value = builder.getInt8(node.value);
		else value = builder.getInt32(node.value);
	}

	virtual void visit(StringLiteral &node) { value = builder.CreateGlobalStringPtr(node.value); }

	virtual void visit(CastExpression &node) {
		// @todo
	}

	virtual void visit(UnaryExpression &node) {
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
			value = getValue(*node.operand, false);
			auto v = builder.CreateLoad(value, "load");
			value = builder.CreateStore(builder.CreateAdd(v, matchType(builder.getInt8(1), v->getType())), value);
		}; break;
		}
	}

	void createLogicalAnd(BinaryExpression &node) {
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

	void createLogicalOr(BinaryExpression &node) {
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

	virtual void visit(BinaryExpression &node) {
		using Op = lexer::Token::Punctuator;
		using Prec = lexer::Token::Precedence;

		auto prec = lexer::Token::precedence(node.op);

		if (prec == Prec::LOGICAL_AND) return createLogicalAnd(node);
		if (prec == Prec::LOGICAL_OR) return createLogicalOr(node);

		auto lhs = getValue(*node.lhs, prec != Prec::ASSIGNMENT);
		auto rhs = getValue(*node.rhs);
		auto type = lhs->getType(); // @todo createType from annotation

		if (prec == Prec::ASSIGNMENT) type = type->getPointerElementType();

/*
		auto lhsType = ((TypePair *)node.lhs->annotation.get())->type;
		auto rhsType = ((TypePair *)node.lhs->annotation.get())->type;
*/

		switch (node.op) {
		// assignments @todo
		case Op::ASSIGN: {
			builder.CreateStore(matchType(rhs, type), lhs);
			value = lhs;
		}; break;

		// relational
		case Op::CMP_LTE: value = builder.CreateICmpSLE(lhs, rhs, "cmp"); break;
		case Op::AB_OPEN: value = builder.CreateICmpSLT(lhs, rhs, "cmp"); break;
		case Op::CMP_GTE: value = builder.CreateICmpSGE(lhs, rhs, "cmp"); break;
		case Op::AB_CLOSE: value = builder.CreateICmpSGT(lhs, rhs, "cmp"); break;
		case Op::CMP_EQ: value = builder.CreateICmpEQ(lhs, rhs, "cmp"); break;
		case Op::CMP_NEQ: value = builder.CreateICmpNE(lhs, rhs, "cmp"); break;

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
		}
	}

	virtual void visit(ConditionalExpression &node) {
	}

	virtual void visit(ExpressionList &node) {
		for (auto &child : node.children) inspect(child);
	}

	virtual void visit(CallExpression &node) {
		std::vector<llvm::Value *> args;
		for (const auto &arg : node.arguments) args.push_back(getValue(*arg));
		value = builder.CreateCall(getValue(*node.function, false), args, "result");
	}

	virtual void visit(SubscriptExpression &node) {
		auto lhs = getValue(*node.base), rhs = getValue(node.subscript);
		bool lptr = lhs->getType()->isPointerTy();
		value = builder.CreateGEP(lptr ? lhs : rhs, lptr ? rhs : lhs, "gep");
		if (shouldLoad) value = builder.CreateLoad(value, "subs");
	}

	virtual void visit(MemberExpression &node) {
		// @todo
	}

	virtual void visit(PostExpression &node) {
		// @todo
	}

	virtual void visit(ExpressionStatement &node) {
		visit(node.expressions);
	}

	virtual void visit(SizeofExpressionTypeName &node) {
		auto &type = ((TypePair *)node.annotation.get())->type;
		value = builder.getInt32((uint32_t)type->getSize(lexer::TextPosition())); // @todo
	}

	virtual void visit(SizeofExpressionUnary &node) {
		auto &type = ((TypePair *)node.annotation.get())->type;
		value = builder.getInt32((uint32_t)type->getSize(lexer::TextPosition()));
	}

	virtual void visit(ComposedTypeSpecifier &) {}
	virtual void visit(NamedTypeSpecifier &) {}

	virtual void visit(IterationStatement &node) {
		auto header = llvm::BasicBlock::Create(ctx, "while-header", func, 0);
		auto body = llvm::BasicBlock::Create(ctx, "while-body", func, 0);
		auto end = llvm::BasicBlock::Create(ctx, "while-end", func, 0);

		builder.CreateBr(header);
		builder.SetInsertPoint(header);
		builder.CreateCondBr(getValue(node.condition), body, end);
		builder.SetInsertPoint(body);
		inspect(node.body);
		builder.CreateBr(header);
		builder.SetInsertPoint(end);
	}

	virtual void visit(SelectionStatement &node) {
		auto header = llvm::BasicBlock::Create(ctx, "if-header", func, 0);
		auto ifTrue = llvm::BasicBlock::Create(ctx, "if-true", func, 0);
		auto end = llvm::BasicBlock::Create(ctx, "if-end", func, 0);
		auto ifFalse = node.when_false.get() ? llvm::BasicBlock::Create(ctx, "if-false", func, 0) : end;

		builder.CreateBr(header);
		builder.SetInsertPoint(header);
		builder.CreateCondBr(getValue(node.condition), ifTrue, ifFalse);

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

	virtual void visit(ReturnStatement &node) {
		builder.CreateRet(getValue(node.expressions));

		auto deadBlock = llvm::BasicBlock::Create(ctx, "dead-block", func, 0);
		builder.SetInsertPoint(deadBlock);
	}
};

#endif
