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

void BlockScope::declareVariable(std::string name, Ptr<Type> type, lexer::TextPosition pos, bool isDefined) {
	if (isDefined) {
		for (auto &d : definitions)
			if (d.first == name) {
				// @todo "redefinition of 'x' with a different type: 'int *' vs 'int **'"
				throw AnalyzerError("redefinition of '" + name + "'", pos);
			}

		definitions.insert(std::make_pair(name, true));
	}

	for (auto &v : variables)
		if (v.first == name && !v.second->isCompatible(*type)) {
			// @todo "redefinition of 'x' with a different type: 'int *' vs 'int **'"
			throw AnalyzerError("conflicting types for '" + name + "'", pos);
		}

	variables.insert(std::make_pair(name, type));
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
