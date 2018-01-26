#ifndef Analyzer_h
#define Analyzer_h

#include "AST.h"

#include <iostream>
#include <functional>
#include <set>
#include <vector>
#include <map>
#include <stack>

using namespace ast;

class AnalyzerError {
public:
	std::string message;
	lexer::TextPosition pos;

	AnalyzerError(const std::string &message, lexer::TextPosition pos)
	: message(message), pos(pos) {}
};

class Scope {
public:
	virtual void open() {};
	virtual void close() {};
};

class Type;
struct DeclarationRef;

class BlockScope : public Scope {
public:
	std::map<std::string, Ptr<DeclarationRef>> variables;
	std::map<std::string, bool> definitions; // @todo could be more elegant

	std::map<std::string, Ptr<Type>> composedTypes;

	bool declaresComposedType(ComposedTypeSpecifier *ct) const {
		return composedTypes.find(ct->name) != composedTypes.end();
	}

	Ptr<Type> resolveComposedType(ComposedTypeSpecifier *ct);

	Ptr<DeclarationRef> declareVariable(std::string name, Ptr<Type> type, lexer::TextPosition pos, bool isDefined);
	bool resolveVariable(std::string name, Ptr<DeclarationRef> &result) {
		auto it = variables.find(name);
		if (it == variables.end()) return false;
		result = it->second;
		return true;
	}
};

class FileScope : public BlockScope {
public:
	std::vector<std::pair<Ptr<Type>, lexer::TextPosition>> unresolvedTentative;
	virtual void close();
};

class FunctionScope : public BlockScope {
public:
	Ptr<Type> returnType;

	std::set<std::string> resolvedLabels;
	std::map<std::string, lexer::TextPosition> unresolvedLabels;

	void resolveLabel(std::string id, lexer::TextPosition pos) {
		if (resolvedLabels.find(id) != resolvedLabels.end())
			throw AnalyzerError("redefinition of label '" + id + "'", pos);

		resolvedLabels.insert(id);
		unresolvedLabels.erase(id);
	}

	void referenceLabel(std::string id, lexer::TextPosition pos) {
		if (resolvedLabels.find(id) == resolvedLabels.end())
			unresolvedLabels.insert(std::make_pair(id, pos));
	}

	virtual void close() {
		BlockScope::close();

		if (unresolvedLabels.empty()) return;

		auto lab = *unresolvedLabels.begin();
		throw AnalyzerError("use of undeclared label '" + lab.first + "'", lab.second);
	}
};

class SwitchScope : public Scope {
public:
};

class IterationScope : public Scope {
public:
};

class ScopeStack {
public:
	std::vector<ast::Ptr<Scope>> stack;

	Ptr<Scope> top() { return stack.back(); }

	template<typename T>
	T &push() {
		stack.push_back(std::make_shared<T>());
		top()->open();
		return reinterpret_cast<T &>(*top());
	}

	template<typename T>
	void execute(std::function<void ()> callback) {
		push<T>();
		callback();
		pop();
	}

	void pop() {
		top()->close();
		stack.pop_back();
	}

	bool empty() const { return stack.empty(); }

	template<typename T>
	ast::Ptr<T> find() { // @todo I doubt this is a good idea…
		for (auto it = stack.rbegin(); it != stack.rend(); ++it)
			if (dynamic_cast<T *>(it->get()))
				return reinterpret_cast<ast::Ptr<T> &>(*it);

		return nullptr;
	}

	bool resolveVariable(std::string name, Ptr<DeclarationRef> &result) {
		for (auto it = stack.rbegin(); it != stack.rend(); ++it)
			if (auto bs = dynamic_cast<BlockScope *>(it->get()))
				if (bs->resolveVariable(name, result))
					return true;
		return false;
	}

	Ptr<Type> resolveComposedType(ComposedTypeSpecifier *ct, bool direct = false) {
		if (!direct)
			for (auto it = stack.rbegin(); it != stack.rend(); ++it)
				if (auto bs = dynamic_cast<BlockScope *>(it->get()))
					if (bs->declaresComposedType(ct))
						return bs->resolveComposedType(ct);

		return find<BlockScope>()->resolveComposedType(ct);
	}
};

// @todo type singletons!
class Type : public std::enable_shared_from_this<Type> {
protected:
	static std::set<Type *> typeQueue;

public:
	struct Member {
		int index;
		std::string name;
		Ptr<Type> type;
	};

	static Ptr<Type> ptrdiffType;

	virtual Ptr<Type> dereference(lexer::TextPosition pos) const {
		throw AnalyzerError("indirection requires pointer operand ('" + describe() + "' invalid)", pos);
	}

	virtual Ptr<Type> call(ast::PtrVector<Type>, lexer::TextPosition pos) const {
		throw AnalyzerError("called object type '" + describe() + "' is not a function or function pointer", pos);
	}

	virtual const Member &getMember(std::string, lexer::TextPosition pos) const {
		throw AnalyzerError("member reference base type '" + describe() + "' is not a structure or union", pos);
	}

	Ptr<Type> reference(); // @todo should be const

	static Ptr<Type> create(const PtrVector<TypeSpecifier> &specifiers, lexer::TextPosition pos, ScopeStack &scopes);
	static Ptr<Type> create(const PtrVector<TypeSpecifier> &specifiers, const Declarator &decl, lexer::TextPosition pos, ScopeStack &scopes);

	Ptr<Type> applyDeclarator(Declarator decl, ScopeStack &scopes);

	virtual bool isScalar() const = 0;
	virtual bool isCompatible(const Type &other) const = 0;
	virtual bool isComplete() const { return true; }
	virtual bool isVoidPointer() const { return false; }
	virtual bool isNullPointer() const { return false; }
	virtual bool isVoid() const { return false; }
	virtual bool isFunction() const { return false; }
	virtual bool isArithmetic() const { return false; }
	virtual size_t getSizeOverride() const { return 0; }

	static Ptr<Type> add(Ptr<Type> &a, Ptr<Type> &b, lexer::TextPosition pos);
	static Ptr<Type> subtract(Ptr<Type> &a, Ptr<Type> &b, lexer::TextPosition pos);

	static bool canCompare(const Type &a, const Type &b, bool broad = false);

	virtual std::string describe() const = 0;
};

struct TypePair : Annotation {
	bool lvalue;
	Ptr<Type> type;

	TypePair() {}
	TypePair(bool lvalue, Ptr<Type> type) : lvalue(lvalue), type(type) {}
};

struct DeclarationRef : TypePair {
	DeclarationRef() { lvalue = true; }

	bool isDefined = false;
	lexer::TextPosition pos;
};

class ArithmeticType : public Type {
public:
	enum Size {
		INT   = 4,
		CHAR  = 1
	};

	static Size max(Size a, Size b) { return (Size)((int)a > (int)b ? a : b); }

	Size size;

	ArithmeticType() : size(INT) {}
	ArithmeticType(Size size) : size(size) {}

	virtual bool isScalar() const { return true; }
	virtual bool isArithmetic() const { return true; }

	virtual bool isCompatible(const Type &other) const {
		auto at = dynamic_cast<const ArithmeticType *>(&other);
		return at && at->size == size;
	}

	virtual std::string describe() const {
		return size == INT ? "int" : "char";
	}
};

class NullPointerType : public ArithmeticType {
public:
	virtual bool isCompatible(const Type &other) const;

	virtual Ptr<Type> dereference(lexer::TextPosition pos) const {
		throw AnalyzerError("cannot dereference null pointer", pos);
	}

	virtual std::string name() const { return "nullptr"; }
	virtual bool isNullPointer() const { return true; }
};

class ComposedType : public Type {
protected:
	bool isComplete_ = false;

public:
	std::string name;
	bool hasTag() const { return !name.empty(); }

	lexer::TextPosition pos;
	lexer::Token::Keyword kind;

	std::vector<Member> members; // @todo hashmap

	virtual bool isScalar() const { return false; }
	virtual bool isCompatible(const Type &other) const { return this == &other; }

	virtual const Member &getMember(std::string name, lexer::TextPosition pos) const {
		if (!isComplete())
			throw AnalyzerError("member access into incomplete type", pos);

		for (auto &member : members) // @todo not efficient
			if (member.name == name)
				return member;

		throw AnalyzerError("no member named '" + name + "' in '" + describe() + "'", pos);
	}

	void addMember(std::string name, Ptr<Type> type, lexer::TextPosition pos) {
		for (auto &member : members)
			if (member.name == name)
				throw AnalyzerError("member " + name + " redefined", pos);

		Member m;
		m.index = (int)members.size();
		m.name = name;
		m.type = type;
		members.push_back(m);
	}

	void addAnonymousStructure(Ptr<Type> type, lexer::TextPosition pos) {
		auto &ct = dynamic_cast<ComposedType &>(*type);
		for (auto &member : ct.members) {
			// @todo correct error position
			for (auto &m : members)
				if (m.name == member.name)
					throw AnalyzerError("member " + m.name + " redefined", pos);

			Member m = member;
			m.index = (int)members.size();
			members.push_back(m);
		}
	}

	virtual bool isComplete() const { return isComplete_; }
	void markAsComplete() {
		isComplete_ = true;
	}

	virtual std::string describe() const {
		std::string result = kind == lexer::Token::Keyword::STRUCT ? "struct " : "union ";
		if (name.empty())
			// @todo file name
			return result + "(anonymous at " + std::to_string(pos.line) + ":" + std::to_string(pos.column) + ")";
		else
			return result + name;
	}
};

class FunctionType : public Type {
public:
	Ptr<Type> returnType;
	std::vector<Ptr<Type>> parameters;
	bool isVariadic = false;

	virtual bool isScalar() const { return true; } // will evaluate to address
	virtual bool isFunction() const { return true; }

	virtual bool isCompatible(const Type &other) const;

	virtual Ptr<Type> call(ast::PtrVector<Type> argTypes, lexer::TextPosition pos) const {
		if (isVariadic ? (argTypes.size() < parameters.size()) : (argTypes.size() != parameters.size())) {
			std::string q = argTypes.size() > parameters.size() ? "many" : "few"; // @todo message for variadic
			throw AnalyzerError(
				"too " + q + " arguments to function call, expected " +
				std::to_string(parameters.size()) + ", have " +
				std::to_string(argTypes.size())
			, pos);
		}

		for (size_t i = 0; i < parameters.size(); ++i) {
			if (!parameters[i]->isComplete())
				throw AnalyzerError(
					"argument type '" + parameters[i]->describe() + "' is incomplete"
				, pos);

			if (!Type::canCompare(*parameters[i], *argTypes[i], true))
				throw AnalyzerError(
					"passing '" + argTypes[i]->describe() + "' to parameter of " +
					"incompatible type '" + parameters[i]->describe() + "'"
				, pos);
		}

		return returnType;
	}

	virtual std::string describe() const {
		std::string result = returnType->describe();
		result += "(";

		bool first = true;
		for (auto &param : parameters) {
			if (first) first = false;
			else result += ", ";
			result += param->describe();
		}

		return result + ")";
	}
};

class VoidType : public Type {
public:
	virtual bool isScalar() const { return false; }
	virtual bool isCompatible(const Type &other) const { return other.isVoid(); }
	virtual std::string describe() const { return "void"; }
	virtual bool isVoid() const { return true; }
};

class PointerType : public Type {
public:
	Ptr<Type> base;

	PointerType() {}
	PointerType(Ptr<Type> base) : base(base) {}

	virtual bool isScalar() const { return true; }
	virtual bool isCompatible(const Type &other) const;

	virtual Ptr<Type> dereference(lexer::TextPosition pos) const {
		if (!base->isComplete())
			throw AnalyzerError("incomplete type '" + base->describe() + "' where a complete type is required", pos);
		return base;
	}

	virtual Ptr<Type> call(ast::PtrVector<Type> argTypes, lexer::TextPosition pos) const {
		return base->call(argTypes, pos);
	}

	virtual std::string describe() const { return base->describe() + "*"; }
	virtual bool isVoidPointer() const { return base->isVoid(); }
};

class StringLiteralType : public PointerType {
public:
	size_t length;

	StringLiteralType(size_t length)
	: PointerType(std::make_shared<ArithmeticType>(ArithmeticType::CHAR)), length(length) {}

	virtual size_t getSizeOverride() const { return length+1; }
};

class ExpressionStack {
public:
	std::stack<TypePair> stack;

	TypePair pop() {
		TypePair t = stack.top();
		stack.pop();
		return t;
	}

	TypePair &push(TypePair t) {
		stack.push(t);
		return top();
	}

	TypePair &top() { return stack.top(); }
};

class Analyzer : public Visitor, public Stream<ast::Ptr<ast::External>, ast::Ptr<ast::External>> {
protected:
	ScopeStack scopes;

	void inspect(Node &node) {
		//std::cout << node.pos.line << ":" << node.pos.column << std::endl;
		node.accept(*this);
	}

	template<typename T>
	void inspect(Ptr<T> &node) {
		if (node.get()) node->accept(*this);
	}

	[[noreturn]] void error(std::string message, Node &node) {
		throw AnalyzerError(message, node.pos);
	}

	TypePair &exprType(Expression &expr) { // @todo assert stack size
		inspect(expr);
		return *(TypePair *)expr.annotation.get();
	}

	Ptr<Annotation> intType, charType, stringType, voidType, nullptrType;

	void initTypes() {
		auto ct = new TypePair(false, std::make_shared<ArithmeticType>(ArithmeticType::CHAR));
		intType.reset(new TypePair(false, std::make_shared<ArithmeticType>(ArithmeticType::INT)));
		charType.reset(ct);
		stringType.reset(new TypePair(false, std::make_shared<PointerType>(ct->type)));
		voidType.reset(new TypePair(false, std::make_shared<VoidType>()));
		nullptrType.reset(new TypePair(false, std::make_shared<NullPointerType>()));
	}

public:
	Analyzer(Source<ast::Ptr<ast::External>> *source)
	: Stream<ast::Ptr<ast::External>, ast::Ptr<ast::External>>(source) {
		initTypes();
		open();
	}

	virtual bool next(ast::Ptr<ast::External> *result) {
		if (this->source->next(result)) {
			inspect(*result);
			return true;
		} else {
			if (!scopes.empty()) close();
			return false;
		}
	}

protected:
	void open() { scopes.push<FileScope>(); }
	void close() { scopes.pop(); }

	void declaration(Declaration &node, bool isGlobal);
	void visitBlockItems(CompoundStatement &node);
	void createLabels(PtrVector<Label> &labels);
	Ptr<Type> typeFromTypeName(TypeName &node);

	virtual void visit(REPLStatement &node);
	virtual void visit(GotoStatement &node);
	virtual void visit(ContinueStatement &node);
	virtual void visit(CompoundStatement &node);
	virtual void visit(IterationStatement &node);
	virtual void visit(SelectionStatement &node);
	virtual void visit(ReturnStatement &node);

	virtual void visit(CaseLabel &node);
	virtual void visit(DefaultLabel &node);
	virtual void visit(IdentifierLabel &node);

	virtual void visit(TypeName &node);
	virtual void visit(Declaration &node);

	virtual void visit(GlobalVariable &node);
	virtual void visit(Function &node);
	virtual void visit(ParameterDeclaration &node);

	virtual void visit(IdentifierExpression &node);
	virtual void visit(Constant &node);
	virtual void visit(StringLiteral &node);
	virtual void visit(CastExpression &node);
	virtual void visit(UnaryExpression &node);
	virtual void visit(BinaryExpression &node);
	virtual void visit(ConditionalExpression &node);
	virtual void visit(ExpressionList &node);
	virtual void visit(CallExpression &node);
	virtual void visit(SubscriptExpression &node);
	virtual void visit(MemberExpression &node);
	virtual void visit(PostExpression &node);
	virtual void visit(ExpressionStatement &node);
	virtual void visit(SizeofExpressionTypeName &node);
	virtual void visit(SizeofExpressionUnary &node);
};

#endif /* Analyzer_h */
