#include <stdio.h>
#include "Analyzer.h"

void FileScope::close() {
	BlockScope::close();

	for (auto &ut : unresolvedTentative)
		if (!ut.first->isComplete())
			throw AnalyzerError(
				"tentative definition has type '" + ut.first->describe()  + "' that is never completed"
			, ut.second);
}

Ptr<DeclarationRef> BlockScope::declareVariable(std::string name, Ptr<Type> type, lexer::TextPosition pos, bool isDefined) {
	for (auto &v : variables) {
		if (v.first != name) continue;

		if (isDefined) {
			if (v.second->isDefined) {
				// @todo "redefinition of 'x' with a different type: 'int *' vs 'int **'"
				throw AnalyzerError("redefinition of '" + name + "'", pos);
			} else {
				v.second->isDefined = true;
			}
		}

		if (!v.second->type->isCompatible(*type)) {
			// @todo "redefinition of 'x' with a different type: 'int *' vs 'int **'"
			throw AnalyzerError("conflicting types for '" + name + "'", pos);
		}

		return v.second;
	}

	auto dr = std::make_shared<DeclarationRef>();
	dr->type = type;
	dr->pos = pos;
	dr->isDefined = isDefined;
	variables.insert(std::make_pair(name, dr));

	return dr;
}

Ptr<Type> BlockScope::resolveComposedType(ComposedTypeSpecifier *ct) {
	auto it = composedTypes.find(ct->name);
	if (it == composedTypes.end()) {
		auto type = std::make_shared<ComposedType>();
		type->name = ct->name;
		type->kind = ct->kind;
		type->pos = ct->pos;

		Ptr<Type> t = type;
		composedTypes.insert(std::make_pair(std::string(ct->name), t));
		return t;
	} else
		return it->second;
}

Ptr<Type> Type::reference() {
	return std::make_shared<PointerType>(shared_from_this());
}

Ptr<Type> Type::create(const PtrVector<TypeSpecifier> &specifiers, lexer::TextPosition pos, ScopeStack &scopes) {
	using Keyword = lexer::Token::Keyword;

	if (specifiers.size() != 1)
		throw AnalyzerError("must have exactly one type specifier", pos);

	auto &spec = *specifiers.begin();
	if (auto nt = dynamic_cast<NamedTypeSpecifier *>(spec.get())) {
		switch (nt->keyword) {
		case Keyword::CHAR: return std::make_shared<ArithmeticType>(ArithmeticType::CHAR);
		case Keyword::INT: return std::make_shared<ArithmeticType>(ArithmeticType::INT);
		case Keyword::VOID: return std::make_shared<VoidType>();

		default:
			throw AnalyzerError("unknown type", spec->pos);
		}
	} else if (auto comp = dynamic_cast<ComposedTypeSpecifier *>(spec.get())) {
		Ptr<Type> c;

		if (comp->isNamed())
			c = scopes.resolveComposedType(comp, comp->isQualified());
		else {
			// anonymous
			c = std::make_shared<ComposedType>();
			auto &cc = dynamic_cast<ComposedType &>(*c);
			cc.kind = comp->kind;
			cc.pos = comp->pos;
		}

		auto &cc = dynamic_cast<ComposedType &>(*c);
		if (!comp->isQualified()) return c;

		bool isNested = typeQueue.find(&cc) != typeQueue.end();
		if (cc.isComplete() || isNested)
			throw AnalyzerError(
				std::string(isNested ? "nested " : "") +
				"redefinition of '" + std::string(comp->name) + "'",
				comp->pos
			);

		typeQueue.insert(&cc);

		for (auto &declaration : comp->declarations) {
			Ptr<Type> type = Type::create(declaration.specifiers, declaration.pos, scopes);
			if (declaration.declarators.empty()) {
				auto subcc = dynamic_cast<ComposedType *>(type.get());
				if (subcc && !subcc->hasTag()) {
					cc.addAnonymousStructure(type, declaration.pos);
				} else if (!subcc)
					throw AnalyzerError("declaration does not declare anything", declaration.pos);
			} else
				for (auto &decl : declaration.declarators) {
					Ptr<Type> dtype = type->applyDeclarator(decl, scopes);
					if (!dtype->isComplete())
						throw AnalyzerError("field has incomplete type '" + dtype->describe() + "'", decl.pos);

					cc.addMember(std::string(decl.name), dtype, decl.pos);
				}
		}

		typeQueue.erase(&cc);
		cc.markAsComplete();

		return c;
	} else
		// unknown AST type
		throw AnalyzerError("unimplemented", spec->pos);
}

Ptr<Type> Type::create(const PtrVector<TypeSpecifier> &specifiers, const Declarator &decl, lexer::TextPosition pos, ScopeStack &scopes) {
	return create(specifiers, pos, scopes)->applyDeclarator(decl, scopes);
}

Ptr<Type> Type::applyDeclarator(Declarator decl, ScopeStack &scopes) {
	Ptr<Type> result = shared_from_this();

	for (auto &mod : decl.modifiers) {
		if (auto pt = dynamic_cast<DeclaratorPointer *>(mod.get())) {
			auto p = std::make_shared<PointerType>();
			p->base = result;
			result = p;
		} else if (auto plist = dynamic_cast<DeclaratorParameterList *>(mod.get())) {
			auto p = std::make_shared<FunctionType>();
			for (auto &param : plist->parameters)
				p->parameters.push_back(create(param.specifiers, param.declarator, param.pos, scopes));

			p->returnType = result;
			p->isVariadic = plist->isVariadic;
			result = p;
		} else
			// unknown modifier
			throw AnalyzerError("unknown modifier", mod->pos);
	}

	return result;
}

Ptr<Type> Type::add(Ptr<Type> &a, Ptr<Type> &b, lexer::TextPosition pos) {
	auto arA = dynamic_cast<ArithmeticType *>(a.get());
	auto arB = dynamic_cast<ArithmeticType *>(b.get());

	if (arA && arB) {
		auto size = ArithmeticType::max(arA->size, arB->size);
		if (arA->size == size) return a;
		if (arB->size == size) return b;
		return std::make_shared<ArithmeticType>(size);
	}

	auto ptrA = dynamic_cast<PointerType *>(a.get());
	auto ptrB = dynamic_cast<PointerType *>(b.get());

	if (arA && ptrB) return b;
	if (ptrA && arB) return a;

	throw AnalyzerError("cannot add incompatible types", pos);
}

Ptr<Type> Type::subtract(Ptr<Type> &a, Ptr<Type> &b, lexer::TextPosition pos) {
	auto arA = dynamic_cast<ArithmeticType *>(a.get());
	auto arB = dynamic_cast<ArithmeticType *>(b.get());

	if (arA && arB) { // @todo not DRY
		auto size = ArithmeticType::max(arA->size, arB->size);
		if (arA->size == size) return a;
		if (arB->size == size) return b;
		return std::make_shared<ArithmeticType>(size);
	}

	auto ptrA = dynamic_cast<PointerType *>(a.get());
	auto ptrB = dynamic_cast<PointerType *>(b.get());
	if (ptrA && arB) return a;

	if (ptrA && ptrB) {
		if (!ptrA->base->isCompatible(*ptrB->base))
			throw AnalyzerError("subtracting incompatible pointer types", pos);
		return Type::ptrdiffType;
	}

	throw AnalyzerError("cannot subtract incompatible types", pos);
}

bool Type::canCompare(const Type &a, const Type &b, bool broad) {
	if (broad && a.isCompatible(b)) return true;
	if (a.isArithmetic() && b.isArithmetic()) return true;

	auto ptrA = dynamic_cast<const PointerType *>(&a);
	auto ptrB = dynamic_cast<const PointerType *>(&b);
	if (ptrA && ptrB) {
		if (ptrA->base->isCompatible(*ptrB->base)) return true;
		if (ptrA->isVoidPointer() || ptrB->isVoidPointer()) return true;
	}

	if (ptrA && b.isNullPointer()) return true;
	if (a.isNullPointer() && ptrB) return true;

	return false;
}

Ptr<Type> Type::ptrdiffType = std::make_shared<ArithmeticType>(ArithmeticType::INT);
std::set<Type *> Type::typeQueue = std::set<Type *>();

bool FunctionType::isCompatible(const Type &other) const {
	if (auto p = dynamic_cast<const PointerType *>(&other)) return p->base->isCompatible(*this);
	if (auto f = dynamic_cast<const FunctionType *>(&other)) {
		if (parameters.size() != f->parameters.size()) return false;
		for (size_t i = 0; i < parameters.size(); ++i)
			if (!parameters[i]->isCompatible(*f->parameters[i])) // @todo isEqual
				return false;

		return returnType->isCompatible(*f->returnType);
	}

	return false;
}

bool PointerType::isCompatible(const Type &other) const {
	if (dynamic_cast<const NullPointerType *>(&other)) return true;
	if (dynamic_cast<const FunctionType *>(&other)) return other.isCompatible(*this);

	auto p = dynamic_cast<const PointerType *>(&other);
	if (!p) return false;
	return base->isCompatible(*p->base);
}

bool NullPointerType::isCompatible(const Type &other) const {
	return dynamic_cast<const ArithmeticType *>(&other) || dynamic_cast<const PointerType *>(&other);
}

#pragma mark - Analyzer

void Analyzer::visit(REPLStatement &node) { inspect(node.statement); }

void Analyzer::visit(CaseLabel &node) {
	if (!scopes.find<SwitchScope>().get()) error("'case' statement not in switch statement", node);
}

void Analyzer::visit(DefaultLabel &node) {
	if (!scopes.find<SwitchScope>().get()) error("'default' statement not in switch statement", node);
}

void Analyzer::visit(IdentifierLabel &node) {
	auto &scope = *scopes.find<FunctionScope>();
	scope.resolveLabel(node.id, node.pos);
}

void Analyzer::createLabels(PtrVector<Label> &labels) {
	for (auto &lab : labels) inspect(lab);
}

void Analyzer::visit(GotoStatement &node) {
	createLabels(node.labels);

	auto &scope = *scopes.find<FunctionScope>();
	scope.referenceLabel(node.target, node.pos); // @todo target pos
}

void Analyzer::visit(ContinueStatement &node) {
	createLabels(node.labels);

	if (!scopes.find<IterationScope>().get())
		error(
			std::string("'") +
			(node.keyword == lexer::Token::Keyword::BREAK ? "break" : "continue") +
			"' statement not in loop statement",
			node
		);
}

void Analyzer::visitBlockItems(CompoundStatement &node) {
	for (auto &item : node.items) inspect(item);
}

void Analyzer::visit(CompoundStatement &node) {
	createLabels(node.labels);

	scopes.execute<BlockScope>([&]() {
		visitBlockItems(node);
	});
}

Ptr<Type> Analyzer::typeFromTypeName(TypeName &node) {
	return Type::create(node.specifiers, node.declarator, node.pos, scopes);
}

void Analyzer::visit(TypeName &node) {
	typeFromTypeName(node);
}

void Analyzer::visit(Declaration &node) { declaration(node, false); }
void Analyzer::declaration(Declaration &node, bool isGlobal) {
	auto scope = scopes.find<BlockScope>();
	auto specifiers = node.specifiers;

	Ptr<Type> type = Type::create(specifiers, node.pos, scopes);
	if (node.declarators.empty()) {
		for (auto &spec : node.specifiers)
			if (auto ct = dynamic_cast<const ComposedTypeSpecifier *>(spec.get()))
				if (ct->isNamed() && ct->isQualified())
					// some composed type was declared, this is valid.
					return;

		return; // @wtf apparently, the tests regard this as semantically legal
		//error("declaration does not declare anything", node);
	}

	for (auto &decl : node.declarators) {
		Ptr<Type> dtype;
		scopes.execute<FunctionScope>([&]() {
			inspect(decl);
			dtype = type->applyDeclarator(decl, scopes);
		});

		if (!dtype->isComplete()) {
			if (isGlobal) {
				auto scope = scopes.find<FileScope>();
				scope->unresolvedTentative.push_back(std::make_pair(dtype, decl.pos));
			} else
				error("variable has incomplete type '" + dtype->describe() + "'", node);
		}

		// find identifier
		if (decl.isAbstract())
			// @todo assert(false) here
			error("abstract declarator in declaration", node);
		else {
			bool isDefinition = decl.initializer.get();
			if (!isGlobal && !dtype->isFunction()) isDefinition = true;
			decl.annotate(scope->declareVariable(decl.name, dtype, decl.pos, isDefinition));
		}

		if (decl.initializer.get()) {
			auto itp = exprType(*decl.initializer);
			if (!Type::canCompare(*itp.type, *dtype, true))
				error(
					"initializing '" + dtype->describe() + "' with an expression of " +
					"incompatible type '" + itp.type->describe() + "'",
					*decl.initializer
				);
		}
	}
}

void Analyzer::visit(GlobalVariable &node) {
	declaration(node.declaration, true);
}

void Analyzer::visit(Function &node) {
	auto &decl = node.declaration.declarators.front();
	if (decl.modifiers.empty())
		error("expected ';' after top level declarator", node);

	auto t = Type::create(node.declaration.specifiers, node.pos, scopes);

	auto scope = scopes.find<BlockScope>();
	scopes.execute<FunctionScope>([&]() {
		scopes.execute<FunctionScope>([&]() {
			// structs need to have FunctionScope
			// @todo not DRY with Declaration &, also: not very elegant
			t = t->applyDeclarator(decl, scopes);
		});

		auto fn = dynamic_cast<FunctionType *>(t.get());
		if (!fn->returnType->isComplete())
			error("incomplete result type in function definition", node);

		decl.annotate(scope->declareVariable(decl.name, t, node.pos, true));

		auto scope = scopes.find<FunctionScope>(); // @todo as callback param?
		scope->returnType = fn->returnType;

		if (auto plist = dynamic_cast<DeclaratorParameterList *>(decl.modifiers.back().get())) {
			for (auto &param : plist->parameters) {
				if (param.declarator.isAbstract())
					error("parameter name omitted", param);

				inspect(param);
			}
		} // @todo else assert(false);

		for (auto &declaration : node.declarations)
			inspect(declaration);

		visitBlockItems(node.body);
	});
}

void Analyzer::visit(ParameterDeclaration &node) {
	Ptr<Type> type = Type::create(node.specifiers, node.declarator, node.pos, scopes);

	if (node.declarator.isAbstract()) return;

	auto scope = scopes.find<FunctionScope>();
	node.annotate(scope->declareVariable(node.declarator.name, type, node.declarator.pos, true));
}

void Analyzer::visit(IdentifierExpression &node) {
	Ptr<DeclarationRef> dr;
	if (!scopes.resolveVariable(node.text, dr))
		error("use of undeclared identifier '" + std::string(node.text) + "'", node);

	node.annotate(dr);
}

void Analyzer::visit(Constant &node) { node.annotate(node.isChar ? charType : (node.value ? intType : nullptrType)); }
void Analyzer::visit(StringLiteral &node) { node.annotate(stringType); }

void Analyzer::visit(CastExpression &node) {
	auto tp = exprType(*node.expression);

	auto &target = node.type;
	Ptr<Type> type = typeFromTypeName(target);

	if (type->isVoid())
		node.annotate(new TypePair(false, type));
	else if (!tp.type->isScalar())
		error("operand of type '" + tp.type->describe() + "' where arithmetic or pointer type is required", node);
	else
		node.annotate(new TypePair(tp.lvalue, type));
}

void Analyzer::visit(UnaryExpression &node) {
	using Punctuator = lexer::Token::Punctuator;

	auto tp = exprType(*node.operand);

	switch (node.op) {
	case Punctuator::ASTERISK:
		node.annotate(new TypePair(true, tp.type->dereference(node.pos)));
		break;

	case Punctuator::BIT_AND:
		if (!tp.lvalue)
			error("cannot take the address of an rvalue of type '" + tp.type->describe() + "'", node);
		node.annotate(new TypePair(false, tp.type->reference()));
		break;

	default:
		// @todo
		node.annotate(new TypePair(tp.lvalue, tp.type));
		break;
	}
}

void Analyzer::visit(BinaryExpression &node) {
	using namespace lexer;
	using Op = lexer::Token::Punctuator;
	using Prec = lexer::Token::Precedence;

	auto lhs = exprType(*node.lhs);
	auto rhs = exprType(*node.rhs);

	switch (node.op) {
	case Op::PLUS: node.annotate(new TypePair(false, Type::add(lhs.type, rhs.type, node.pos))); break;
	case Op::MINUS: node.annotate(new TypePair(false, Type::subtract(lhs.type, rhs.type, node.pos))); break;
	default:
		switch (Token::precedence(node.op)) {
		case Prec::MULTIPLICATIVE: // integer for %, arithmetic otherwise
		case Prec::SHIFT: // integer
		case Prec::AND:
		case Prec::INCLUSIVE_OR:
		case Prec::EXCLUSIVE_OR:
		{
			if (!lhs.type->isArithmetic() || !rhs.type->isArithmetic())
				error(
					"invalid operands to binary expression ('" + lhs.type->describe() +
					"' and '" + rhs.type->describe() + "')",
					node
				);
			node.annotate(new TypePair(false, lhs.type)); // @todo?
			break;
		}

		case Prec::LOGICAL_OR:
		case Prec::LOGICAL_AND:
			if (!lhs.type->isScalar() || !rhs.type->isScalar())
				error(
					"invalid operands to binary expression ('" + lhs.type->describe() +
					"' and '" + rhs.type->describe() + "')",
					node
				);
			node.annotate(intType);
			break;

		case Prec::ASSIGNMENT:
			if (!lhs.lvalue) error("expression is not assignable", node);
			if (!Type::canCompare(*lhs.type, *rhs.type, true))
				error(
					"assigning to '" + lhs.type->describe() +
					"' from incompatible type '" + rhs.type->describe() + "'",
					*node.rhs
				);
			node.annotate(new TypePair(false, lhs.type));
			break;

		case Prec::EQUALITY: {
			if (!Type::canCompare(*lhs.type, *rhs.type))
				error(
					"comparison of distinct pointer types ('" + lhs.type->describe() + "' and '" +
					rhs.type->describe() + "')",
					node
				);
			node.annotate(intType);
			break;
		}

		case Prec::RELATIONAL:
			if (!lhs.type->isCompatible(*rhs.type))
				error(
					"comparison of distinct pointer types ('" + lhs.type->describe() + "' and '" +
					rhs.type->describe() + "')",
					node
				);
			node.annotate(intType);
			break;

		default:
			error("operator not implemented", node);
		}
	}
}

void Analyzer::visit(ConditionalExpression &node) {
	auto cond = exprType(*node.condition);
	if (!cond.type->isScalar())
		error(
			"used type '" + cond.type->describe() +
			"' where arithmetic or pointer type is required", *node.condition
		);

	auto &lhs = exprType(*node.when_true);
	exprType(*node.when_false);

	// @todo compare lhs and rhs type

	node.annotate(new TypePair(lhs.lvalue, lhs.type));
}

void Analyzer::visit(ExpressionList &node) {
	auto lastType = voidType;
	for (auto &child : node.children) {
		inspect(*child);
		lastType = child->annotation;
	}
	node.annotate(lastType);
}

void Analyzer::visit(CallExpression &node) {
	auto tp = exprType(*node.function);

	PtrVector<Type> argumentTypes;
	for (auto &arg : node.arguments) {
		auto tp = exprType(*arg);
		argumentTypes.push_back(tp.type);
	}

	node.annotate(new TypePair(false, tp.type->call(argumentTypes, node.pos)));
}

void Analyzer::visit(SubscriptExpression &node) {
	auto base = exprType(*node.base);
	auto subscript = exprType(node.subscript);

	auto result = Type::add(base.type, subscript.type, node.pos);

	node.annotate(new TypePair(true, result->dereference(node.pos)));
}

void Analyzer::visit(MemberExpression &node) {
	auto base = exprType(*node.base);

	if (node.dereference) {
		base.lvalue = true;
		base.type = base.type->dereference(node.pos);
	}

	node.annotate(new TypePair(base.lvalue, base.type->getMember(node.id, node.pos).type));
}

void Analyzer::visit(PostExpression &node) {
	auto t = exprType(*node.base);
	// @todo test if this makes sense
	node.annotate(new TypePair(false, t.type));
}

void Analyzer::visit(ExpressionStatement &node) {
	createLabels(node.labels);
	exprType(node.expressions);
}

void Analyzer::visit(SizeofExpressionTypeName &node) {
	node.type.annotate(new TypePair(false, typeFromTypeName(node.type)));
	node.annotate(intType);
}

void Analyzer::visit(SizeofExpressionUnary &node) {
	exprType(*node.expression);
	node.annotate(intType);
}

void Analyzer::visit(IterationStatement &node) {
	createLabels(node.labels);

	auto cond = exprType(node.condition);
	if (!cond.type->isScalar())
		error(
			"statement requires expression of scalar type ('" + cond.type->describe() +
			"' invalid)", node.condition
		);

	scopes.execute<IterationScope>([&]() {
		inspect(node.body);
	});
}

void Analyzer::visit(SelectionStatement &node) {
	createLabels(node.labels);

	auto cond = exprType(node.condition);
	if (!cond.type->isScalar())
		error(
			"statement requires expression of scalar type ('" + cond.type->describe() +
			"' invalid)", node.condition
		);

	inspect(node.when_true);
	inspect(node.when_false);
}

void Analyzer::visit(ReturnStatement &node) {
	createLabels(node.labels);

	auto scope = scopes.find<FunctionScope>();
	auto expectedType = scope->returnType;
	auto givenType = exprType(node.expressions);

	if (!Type::canCompare(*expectedType, *givenType.type, true))
		throw AnalyzerError(
			"returning '" + givenType.type->describe() + "' from a function with incompatible " +
			"result type '" + expectedType->describe() + "'",
			node.pos
		);
}
