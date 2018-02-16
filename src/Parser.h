#ifndef Parser_hpp
#define Parser_hpp

#include <stdio.h>
#include <iostream>
#include <vector>
#include <memory>
#include <sstream>

#include "Lexer.h"
#include "AST.h"


extern bool debug_mode;

using namespace ast;
using namespace lexer;

class ParserError : public common::Error {
public:
	ParserError(const std::string &message, common::TextPosition pos)
	: common::Error(message, pos) {}

	[[noreturn]] virtual void raise() { throw *this; }
};

class Parser;
struct DebugTree {
public:
	const char *function = "(root)";
	int start_index = 0, end_index = 0;
	bool ret_val, has_returned = false;

	DebugTree *parent;
	std::vector<std::shared_ptr<DebugTree>> children;

	DebugTree *create_child(const char *function, int index) {
		auto child = std::make_shared<DebugTree>();
		child->function = function;
		child->start_index = index;
		child->parent = this;
		children.push_back(child);

		return child.get();
	}

	DebugTree *perform_return(bool value, int index) {
		has_returned = true;
		ret_val = value;
		end_index = index;

		return parent;
	}

	void dump(Parser *parser, std::string indent = "");
};

/**
 * Used internally for methods that make use of #OPTION.
 * This will notify the parse tree that we are leaving a method.
 */
#define _DEBUG_RETURN(x) { \
	--depth; \
	if (debug_mode) \
		dbg_tree_current = dbg_tree_current->perform_return(x, i); \
	return x; \
}

/**
 * Aborts the current production and backtracks.
 */
#define FAIL goto deny;

/**
 * Breaks out of the current production if no terminals have been read
 * in this production.
 * @see #FAIL
 */
#define FAIL_IF_EMPTY \
	if (i == _initial_i) \
		FAIL

/**
 * Used internally as prologue for productions.
 */
#define _OPTION_PREFIX \
	{ \
		__label__ deny;

/**
 * Used internally as epilogue for productions.
 * A production will by default always succeed, unless there has been a
 * #FAIL statement (including #NON_OPTIONAL, #FAIL_IF_EMPTY, ...) that
 * caused the current production to fail.
 * At the end of a production (whether succesful or not), the error reporting
 * mode will be restored to what it was previously when beginning the production.
 */
#define _OPTION_SUFFIX \
		error_flag = _initial_ef; \
		_DEBUG_RETURN(true) \
		deny: \
		i = _initial_i; \
		error_flag = _initial_ef; \
	}

/**
 * Starts a set of productions. Write this at the start of a method.
 * This will take care of notifing the debug tree that we've entered a method.
 * @see #_OPTION_SUFFIX
 */
#define OPTION { \
	if (debug_mode) { \
		/* printf("%d: %s\n", i, __PRETTY_FUNCTION__); */ \
		dbg_tree_current = dbg_tree_current->create_child(__PRETTY_FUNCTION__, i); \
	} \
	++depth; \
	int _initial_i = i; \
	bool _initial_ef = error_flag; \
	_OPTION_PREFIX;

/**
 * Adds an alternative production to the set of already provided productions.
 * If the preceeding production fails, then this one will be attempted next.
 * @see #_OPTION_SUFFIX
 */
#define ELSE_OPTION  \
	_OPTION_SUFFIX \
	_OPTION_PREFIX

/**
 * Ends the set of productions. Write this at the end of a method.
 * This will only be reached if all productions have failed, and this
 * will therefore cause the method to return false.
 */
#define END_OPTION \
	_OPTION_SUFFIX \
	_DEBUG_RETURN(false) \
}

/**
 * Throws an error with the given message if error reporting is enabled.
 */
#define ERROR(error_message) if (error_flag) error(error_message);

/**
 * Ends the set of productions (similarly to #END_OPTION), but will throw an
 * error with the given message if error reporting is enabled.
 */
#define OTHERWISE_FAIL(error_message) \
	_OPTION_SUFFIX \
	ERROR(error_message) \
	error_flag = _initial_ef; \
	_DEBUG_RETURN(false) \
}

/**
 * Temporarily disables error reporting if necessary, but causes the current production to
 * fail if the provided expression evaluates to false.
 * @see #NON_OPTIONAL
 */
#define ALLOW_FAILURE(expr) \
	{ \
		bool _prev_ef = error_flag; \
		error_flag = false; \
		if (!(expr)) \
			FAIL \
		error_flag = _prev_ef; \
	}

/**
 * Causes the current production to fail if the provided expression evaluates to
 * false.
 * @see ALLOW_FAILURE
 */
#define NON_OPTIONAL(expr) \
	if (!(expr)) \
		FAIL;

/**
 * Enables error reporting, which means that parsing will be stopped as soon as an
 * error is encountered. This is used in passages of the grammar where we can guarantee
 * that the current parse tree is the only parse tree for the prefix we've read so far.
 * Example: Assume we're inside the `if-statement` production of the `statement` non-terminal and
 * the next token in the queue is a `WHILE`-keyword. We try to read an `IF`-keyword, which will error -
 * but we may not abort parsing at this point because there are alternative productions we can still
 * try (namely the `while-statement` production). This is why error reporting would at this point
 * be turned off and the error will cause the current production to silently fail.
 * If, on the other hand, we had already read an `IF`-keyword, we can guarantee that the `if-statement`
 * production will be the only valid production at this point (we know that all alternative parse trees
 * will fail earlier than our current parse tree) so if we now fail to read the `(` that must succeed
 * the `IF`-keyword, we can abort parsing as soon as an error is encountered (error reporting will be
 * turned on after successfully reading the `IF`-keyword).
 */
#define UNIQUE \
	error_flag = true;

/**
 * Disables error reporting.
 * @see #UNIQUE
 */
#define NON_UNIQUE \
	error_flag = false;

/**
 * Disables error reporting, causes the current production to fail if the provided expression
 * evaluates to false or alternatively enables error reporting if it evaluates to true.
 */
#define BEGIN_UNIQUE(expr) \
	NON_UNIQUE \
	if (!(expr)) \
		FAIL \
	else UNIQUE

/**
 * Disables error reporting temporarily if necessary and executes the provided statement.
 */
#define OPTIONAL(stmt) \
	{ \
		bool _prev_ef = error_flag; \
		error_flag = false; \
		stmt; \
		error_flag = _prev_ef; \
	}

/**
 * Uses recursive-descent parsing to transform tokens into external AST nodes.
 */
class Parser : public streams::Stream<Token, Ptr<External>> {
	DebugTree dbg_tree_root;
	DebugTree *dbg_tree_current = &dbg_tree_root;
public:
	bool allowTLS; // allow top level statements

	Parser(Source<Token> *source, bool tls = false)
	: Stream<Token, Ptr<External>>(source), allowTLS(tls) {}

	void reset() {
		i = (int)token_queue.size();
	}

	int depth;
	virtual bool next(Ptr<External> *result) {
		if (peek().kind == Token::Kind::END) {
			if (!this->i)
				ParserError("empty file", common::TextPosition()).raise();
			return false;
		}

		depth = 0;
		if (allowTLS) {
			try {
				Ptr<ExternalDeclaration> decl;
				read_external_declaration(decl);
				*result = decl;
			} catch (ParserError e) {
				auto repl = std::make_shared<REPLStatement>();
				read_statement(repl->statement);
				*result = repl;
			}
		} else {
			Ptr<ExternalDeclaration> decl;
			read_external_declaration(decl);
			*result = decl;
		}

		return true;
	}

	void print_context(int index = -1) {
		if (index < 0) index = i;

		for (int j = -(index >= 5 ? 5 : index); j <= 5; ++j) {
			if (!j) std::cout << "(here) ";
			std::cout << peek(index - i + j).text << " ";
		}

		std::cout << std::endl;
	}

	void print_debug_tree() { dbg_tree_root.dump(this); }

protected:
	int i = 0;

	/**
	 * Whether production failures cause the parsing to be aborted.
	 * See #UNIQUE for details on error reporting.
	 */
	bool error_flag = true;

	bool shift(bool condition = true) {
		i += condition ? 1 : 0;
		return condition;
	}

	std::vector<Token> token_queue;

	bool eof(int offset = 0) {
		return peek(offset).kind == Token::Kind::END;
	}

	Token &peek(int offset = 0) {
		int i = this->i + offset;

		if (i < (int)token_queue.size()) return token_queue[i];

		while ((int)token_queue.size() <= i) {
			Token t;
			this->source->next(&t);
			token_queue.push_back(t);
		}

		return token_queue.back();
	}

	[[noreturn]] void error(const std::string &message, int offset = 0) {
		auto start_pos = peek(offset).pos;
		ParserError(message, start_pos).raise();
	}

#pragma mark - Terminals

	bool read_punctuator(Token::Punctuator punctuator)
	OPTION
		NON_OPTIONAL(peek().punctuator == punctuator)
		shift();
	OTHERWISE_FAIL(std::string(Token::operatorName(punctuator)) + " expected")

	bool read_keyword(Token::Keyword keyword)
	OPTION
		NON_OPTIONAL(peek().keyword == keyword)
		shift();
	OTHERWISE_FAIL("keyword " + std::string(Token::keywordName(keyword)) + " expected");

	bool read_unary_operator(Token::Punctuator &op)
	OPTION
		using P = Token::Punctuator;
		switch (peek().punctuator) {
			case P::BIT_AND: case P::ASTERISK:
			case P::PLUS: case P::MINUS:
			case P::BIT_NOT: case P::LOG_NOT:
				op = peek().punctuator;
				shift();
				break;

			default:
				FAIL
		}
	OTHERWISE_FAIL("unary operator expected")

	bool read_identifier(std::string &text)
	OPTION
		NON_OPTIONAL(peek().kind == Token::Kind::IDENTIFIER)
		text = peek().text;
		shift();
	OTHERWISE_FAIL("identifier expected")

	// @todo not elegant
	bool read_identifier_expression(Ptr<Expression> &node)
	OPTION
		auto c = std::make_shared<IdentifierExpression>();
		NON_OPTIONAL(peek().kind == Token::Kind::IDENTIFIER)
		c->pos = peek().pos;
		c->text = peek().text;
		shift();
		node = c;
	OTHERWISE_FAIL("identifier expected")

	bool read_constant(Ptr<Expression> &node)
	OPTION
		auto c = std::make_shared<Constant>();
		NON_OPTIONAL(peek().kind == Token::Kind::CONSTANT)
		c->pos = peek().pos;
		c->text = peek().text;
		c->value = peek().intValue;
		shift();
		node = c;
	OTHERWISE_FAIL("constant expected")

	bool read_string_literal(Ptr<Expression> &node)
	OPTION
		auto c = std::make_shared<StringLiteral>();
		NON_OPTIONAL(peek().kind == Token::Kind::STRING_LITERAL)
		c->pos = peek().pos;
		c->text = peek().text;
		c->value = peek().stringValue;
		shift();
		node = c;
	OTHERWISE_FAIL("string literal expected")

#pragma mark - Other stuff

	bool read_type_specifier_keyword(NamedTypeSpecifier &node) {
		switch (peek().keyword) {
			case Token::Keyword::VOID:
			case Token::Keyword::CHAR:
			case Token::Keyword::INT:
				node.id = peek().text;
				node.keyword = peek().keyword;
				return shift();

			default:
				return false;
		}
	}

	template<typename T>
	bool read_list(bool (Parser::*method)(T &node), std::vector<T> &result, Token::Punctuator end_marker = Token::Punctuator::NEVER) {
		int initial_i = i;
		bool _initial_ef = error_flag;

		while (!eof()) {
			if (peek().punctuator == end_marker) break;

			T temp;
			if (!(this->*method)(temp)) break;
			result.push_back(temp);
		}

		error_flag = _initial_ef;
		return i != initial_i;
	}

	template<typename T>
	bool read_separated_list(bool (Parser::*method)(T &node), Token::Punctuator separator, std::vector<T> &result, bool strict = true) {
		int initial_i = i;
		int last_good_i = i;
		bool _initial_ef = error_flag;

		while (!eof()) {
			T temp;
			if (!(this->*method)(temp)) break;

			result.push_back(temp);
			last_good_i = i;

			error_flag = false;
			if (!read_punctuator(separator)) break;
			error_flag = strict;
		}

		error_flag = _initial_ef;
		i = last_good_i;
		return i != initial_i;
	}

#pragma mark - Declarations

	bool read_declaration_specifiers(PtrVector<TypeSpecifier> &node)
	OPTION
		NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
	OTHERWISE_FAIL("type specifier expected")

	bool read_declarator_na(Declarator &node) {
		return read_declarator(node, false);
	}

	bool read_declarator(Declarator &node, bool isAbstract)
	OPTION
		node.pos = peek().pos;

		bool has_pointer;
		OPTIONAL(has_pointer = read_pointer(node.modifiers))
		if (has_pointer && isAbstract) {
			OPTIONAL(read_direct_declarator(node, isAbstract))
		} else {
			NON_OPTIONAL(read_direct_declarator(node, isAbstract))
		}
	END_OPTION

	bool read_direct_declarator_prefix(Declarator &node, bool isAbstract)
	OPTION
		ALLOW_FAILURE(!isAbstract && read_identifier(node.name))
	ELSE_OPTION
		ALLOW_FAILURE(read_punctuator(Token::Punctuator::RB_OPEN))
		NON_OPTIONAL(read_declarator(node, isAbstract))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
	END_OPTION

	bool read_direct_declarator(Declarator &node, bool isAbstract)
	OPTION
		int last_good_i;

		size_t insertionIndex = node.modifiers.size(); // @todo not efficient
		if (isAbstract) { // @todo common pattern
			OPTIONAL(read_direct_declarator_prefix(node, isAbstract))
		} else {
			NON_OPTIONAL(read_direct_declarator_prefix(node, isAbstract))
		}

		last_good_i = i;
		while (!eof()) {
			NON_UNIQUE
			auto p_suffix = std::make_shared<DeclaratorParameterList>();
			if (!read_punctuator(Token::Punctuator::RB_OPEN)) break;

			OPTIONAL(read_parameter_type_list(p_suffix->parameters))
			if (read_punctuator(Token::Punctuator::COMMA)) { // @bug this allows (, ...)
				UNIQUE
				NON_OPTIONAL(read_punctuator(Token::Punctuator::ELIPSES))
				p_suffix->isVariadic = true;
			}

			if (p_suffix->parameters.size() == 1 && !p_suffix->isVariadic) {
				auto p = p_suffix->parameters.front();
				if (p.declarator.isAbstract() && p.declarator.modifiers.empty() && p.specifiers.size() == 1) {
					auto s = p.specifiers.front();
					if (auto nt = dynamic_cast<NamedTypeSpecifier *>(s.get()))
						if (nt->id == "void") {
							p_suffix->parameters.clear(); // no parameters taken
							p_suffix->removedVoid = true;
						}
				}
			}

			UNIQUE
			if (!read_punctuator(Token::Punctuator::RB_CLOSE)) break;

			node.modifiers.insert(node.modifiers.begin() + insertionIndex, p_suffix);
			last_good_i = i;
		}

		i = last_good_i;

		FAIL_IF_EMPTY
	OTHERWISE_FAIL("direct declarator expected")

	bool read_type_name(TypeName &node)
	OPTION
		NON_OPTIONAL(read_specifier_qualifier_list(node.specifiers))
		OPTIONAL(read_declarator(node.declarator, true))
	END_OPTION

	bool read_parameter_type_list(Vector<ParameterDeclaration> &node)
	OPTION
		NON_OPTIONAL(read_separated_list(&Parser::read_parameter_declaration, Token::Punctuator::COMMA, node, false))
	END_OPTION

	bool read_parameter_declaration(ParameterDeclaration &node)
	OPTION
		node.pos = peek().pos;

		NON_OPTIONAL(read_declaration_specifiers(node.specifiers))

		NON_UNIQUE
		if (read_declarator(node.declarator, false)) {
		} else {
			node.declarator = Declarator(); // @fixme not elegant
			if (read_declarator(node.declarator, true)) {}
		}
	END_OPTION

	bool read_pointer_single(Ptr<DeclaratorModifier> &node)
	OPTION
		NON_OPTIONAL(read_punctuator(Token::Punctuator::ASTERISK))
		node = std::make_shared<DeclaratorPointer>();
	END_OPTION

	bool read_pointer(PtrVector<DeclaratorModifier> &node)
	OPTION
		NON_OPTIONAL(read_list(&Parser::read_pointer_single, node))
	END_OPTION

	bool read_specifier_qualifier_list(PtrVector<TypeSpecifier> &node)
	OPTION
		NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
	OTHERWISE_FAIL("specifier qualifier list expected")

#pragma mark - Structs

	bool read_struct_declarator_list(Vector<Declarator> &node)
	OPTION
		NON_OPTIONAL(read_separated_list(&Parser::read_declarator_na, Token::Punctuator::COMMA, node))
	END_OPTION

	bool read_struct_declaration(Declaration &node)
	OPTION
		node.pos = peek().pos;
		
		BEGIN_UNIQUE(read_specifier_qualifier_list(node.specifiers))
		OPTIONAL(read_struct_declarator_list(node.declarators))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
	END_OPTION

	bool read_struct_declaration_list(std::vector<Declaration> &node)
	OPTION
		NON_OPTIONAL(read_list(&Parser::read_struct_declaration, node))
	OTHERWISE_FAIL("struct declaration list expected")

	bool read_struct_body(ComposedTypeSpecifier &node)
	OPTION
		NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_OPEN))
		NON_OPTIONAL(read_struct_declaration_list(node.declarations))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_CLOSE))
	END_OPTION

	bool read_type_specifier(Ptr<TypeSpecifier> &node)
	OPTION
		NamedTypeSpecifier type_name;
		if (read_type_specifier_keyword(type_name)) {
			node = std::make_shared<NamedTypeSpecifier>(type_name);
		} else {
			Token &token = peek();
			switch (token.keyword) {
				case Token::Keyword::STRUCT:
				case Token::Keyword::UNION: {
					shift();

					auto n = new ComposedTypeSpecifier();
					node.reset(n);

					n->pos = token.pos;
					n->kind = token.keyword;

					NON_UNIQUE
					bool has_identifier = read_identifier(n->name);
					bool has_body = peek().punctuator == Token::Punctuator::CB_OPEN;

					UNIQUE
					if (has_body) {
						NON_OPTIONAL(read_struct_body(*n))
					} else if (!has_identifier) {
						error("struct/union without identifier or body");
					}

					break;
				}

				default:
					FAIL
			}
		}
	END_OPTION

	bool read_initializer(Declarator &node)
	OPTION
		Ptr<Expression> assignment_expr;
		ALLOW_FAILURE(read_assignment_expression(assignment_expr))
		node.initializer = assignment_expr;
	OTHERWISE_FAIL("initializer expected")

	bool read_init_declarator(Declarator &node)
	OPTION
		NON_OPTIONAL(read_declarator(node, false))

		NON_UNIQUE
		if (read_punctuator(Token::Punctuator::ASSIGN)) {
			UNIQUE
			NON_OPTIONAL(read_initializer(node));
		}
	END_OPTION

	bool read_init_declarator_list(Vector<Declarator> &node)
	OPTION
		NON_OPTIONAL(read_separated_list(&Parser::read_init_declarator, Token::Punctuator::COMMA, node))
	END_OPTION

	bool read_declaration(Declaration &node)
	OPTION
		node.pos = peek().pos;

		BEGIN_UNIQUE(read_declaration_specifiers(node.specifiers))
		OPTIONAL(read_init_declarator_list(node.declarators))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
	END_OPTION

	bool read_declaration_list(std::vector<Declaration> &node)
	OPTION
		NON_OPTIONAL(read_list(&Parser::read_declaration, node))
	END_OPTION

	bool read_external_declaration(Ptr<ExternalDeclaration> &node)
	OPTION
		NON_UNIQUE
		
		auto pos = peek().pos;

		PtrVector<TypeSpecifier> specifiers;
		Declarator declarator;
		bool has_declarator, needs_declaration_list, needs_initialization, is_declaration;
		bool is_external = read_keyword(Token::Keyword::EXTERN);

		UNIQUE
		NON_OPTIONAL(read_declaration_specifiers(specifiers))
		NON_UNIQUE

		has_declarator = read_declarator(declarator, false);
		needs_declaration_list = has_declarator && peek().punctuator == Token::Punctuator::COMMA;
		needs_initialization = has_declarator && peek().punctuator == Token::Punctuator::ASSIGN;
		is_declaration = needs_initialization || needs_declaration_list || peek().punctuator == Token::Punctuator::SEMICOLON;

		UNIQUE
		if (is_declaration) {
			// declaration: ... (',' init-declarator-list(opt))(opt) ;

			auto n = new GlobalVariable();
			node.reset(n);

			n->declaration.specifiers = std::move(specifiers);
			n->declaration.isExternal = is_external;

			if (needs_initialization) {
				shift(); // consume assign
				read_initializer(declarator);

				needs_declaration_list = peek().punctuator == Token::Punctuator::COMMA;
			}

			if (has_declarator)
				n->declaration.declarators.push_back(std::move(declarator));

			if (needs_declaration_list) {
				shift(); // jump over comma
				read_init_declarator_list(n->declaration.declarators);
			}

			NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
		} else {
			// function definition: ... declaration-list(opt) compound-statement

			if (!has_declarator)
				read_declarator(declarator, false);

			auto n = new Function();
			node.reset(n);

			n->declaration.specifiers = std::move(specifiers);
			n->declaration.declarators.push_back(std::move(declarator));

			OPTIONAL(read_declaration_list(n->declarations))
			NON_OPTIONAL(read_compound_statement(n->body))
		}

		node->pos = pos;
	END_OPTION

#pragma mark - Statements

	bool read_statement(Ptr<Statement> &node)
	{
	auto pos = peek().pos;
	OPTION
		ALLOW_FAILURE(read_labeled_statement(node))
	ELSE_OPTION
		auto c = std::make_shared<CompoundStatement>();
		ALLOW_FAILURE(read_compound_statement(*c))
		c->pos = pos;
		node = c;
	ELSE_OPTION
		auto e = std::make_shared<ExpressionStatement>();
		ALLOW_FAILURE(read_expression_statement(*e))
		e->pos = pos;
		node = e;
	ELSE_OPTION
		Ptr<SelectionStatement> s;
		ALLOW_FAILURE(read_selection_statement(s))
		s->pos = pos;
		node = s;
	ELSE_OPTION
		Ptr<IterationStatement> stmt;
		ALLOW_FAILURE(read_iteration_statement(stmt))
		stmt->pos = pos;
		node = stmt;
	ELSE_OPTION
		Ptr<JumpStatement> j;
		ALLOW_FAILURE(read_jump_statement(j))
		j->pos = pos;
		node = j;
	OTHERWISE_FAIL("statement expected")
	}

	bool read_labeled_statement(Ptr<Statement> &node)
	OPTION
		Ptr<Label> label;
		auto pos = peek().pos;

		if (read_keyword(Token::Keyword::CASE)) {
			auto n = std::make_shared<CaseLabel>();
			NON_OPTIONAL(read_constant_expression(n->expression))
			label = n;
		} else if (read_keyword(Token::Keyword::DEFAULT)) {
			label = std::make_shared<DefaultLabel>();
		} else if (peek(0).kind == Token::Kind::IDENTIFIER) {
			auto n = std::make_shared<IdentifierLabel>();
			read_identifier(n->id);
			label = n;
		} else
			FAIL

		label->pos = pos;

		NON_OPTIONAL(read_punctuator(Token::Punctuator::COLON))

		UNIQUE
		NON_OPTIONAL(read_statement(node))

		node->labels.insert(node->labels.begin(), label); // @todo not efficient!
	END_OPTION

	bool read_block_item(Ptr<BlockItem> &node)
	OPTION
		auto n = std::make_shared<Declaration>();
		NON_OPTIONAL(read_declaration(*n))
		node = n;
	ELSE_OPTION
		Ptr<Statement> stmt;
		NON_OPTIONAL(read_statement(stmt))
		node = stmt;
	END_OPTION

	bool read_block_item_list(PtrVector<BlockItem> &node)
	OPTION
		NON_OPTIONAL(read_list(&Parser::read_block_item, node, Token::Punctuator::CB_CLOSE))
	END_OPTION

	bool read_compound_statement(CompoundStatement &node)
	OPTION
		BEGIN_UNIQUE(read_punctuator(Token::Punctuator::CB_OPEN))
		OPTIONAL(read_block_item_list(node.items))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_CLOSE))
	OTHERWISE_FAIL("compound statement expected")

	bool read_expression_statement(ExpressionStatement &node)
	OPTION
		bool has_expression = read_expression(node.expressions);
		error_flag = error_flag || has_expression;
		NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
	END_OPTION

	bool read_selection_statement(Ptr<SelectionStatement> &node)
	OPTION
		BEGIN_UNIQUE(read_keyword(Token::Keyword::IF))

		node = std::make_shared<SelectionStatement>();

		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
		NON_OPTIONAL(read_expression(node->condition))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
		NON_OPTIONAL(read_statement(node->when_true))

		NON_UNIQUE
		if (read_keyword(Token::Keyword::ELSE)) {
			UNIQUE
			NON_OPTIONAL(read_statement(node->when_false))
		}
	END_OPTION

	bool read_iteration_statement(Ptr<IterationStatement> &node)
	OPTION
		BEGIN_UNIQUE(read_keyword(Token::Keyword::WHILE))

		node = std::make_shared<IterationStatement>();

		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
		NON_OPTIONAL(read_expression(node->condition))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
		NON_OPTIONAL(read_statement(node->body))
	END_OPTION

	bool read_jump_statement(Ptr<JumpStatement> &node)
	OPTION
		auto g = std::make_shared<GotoStatement>();

		BEGIN_UNIQUE(read_keyword(Token::Keyword::GOTO))
		NON_OPTIONAL(read_identifier(g->target))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))

		node = g;
	ELSE_OPTION
		Token::Keyword keyword = peek().keyword;
		Ptr<ContinueStatement> c;

		BEGIN_UNIQUE(read_keyword(Token::Keyword::CONTINUE) || read_keyword(Token::Keyword::BREAK))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))

		c = std::make_shared<ContinueStatement>();
		c->keyword = keyword;
		node = c;
	ELSE_OPTION
		auto r = std::make_shared<ReturnStatement>();

		BEGIN_UNIQUE(read_keyword(Token::Keyword::RETURN))
		OPTIONAL(read_expression(r->expressions))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))

		node = r;
	END_OPTION

#pragma mark - Expressions

	bool read_expression(ExpressionList &node)
	OPTION
		node.pos = peek().pos;
		NON_OPTIONAL(read_separated_list(&Parser::read_assignment_expression, Token::Punctuator::COMMA, node.children))
	END_OPTION

	bool read_primary_expression(Ptr<Expression> &node)
	OPTION
		NON_OPTIONAL(read_identifier_expression(node) || read_constant(node) || read_string_literal(node))
	ELSE_OPTION
		auto list = std::make_shared<ExpressionList>();
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
		NON_OPTIONAL(read_expression(*list))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
		node = list;
	END_OPTION

	bool read_argument_expression_list(PtrVector<Expression> &node)
	OPTION
		NON_OPTIONAL(read_separated_list(&Parser::read_assignment_expression, Token::Punctuator::COMMA, node))
	END_OPTION

	bool read_postfix_expression(Ptr<Expression> &node)
	OPTION
		NON_OPTIONAL(read_primary_expression(node))

		while (!eof()) {
			auto pos = peek().pos;

			NON_UNIQUE
			if (read_punctuator(Token::Punctuator::SB_OPEN)) {
				UNIQUE

				auto s = std::make_shared<SubscriptExpression>();
				s->base = node;

				NON_OPTIONAL(read_expression(s->subscript))
				NON_OPTIONAL(read_punctuator(Token::Punctuator::SB_CLOSE))

				node = s;
			} else if (read_punctuator(Token::Punctuator::RB_OPEN)) {
				UNIQUE

				auto n = std::make_shared<CallExpression>();
				n->function = node;

				OPTIONAL(read_argument_expression_list(n->arguments))
				NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))

				node = n;
			} else if (read_punctuator(Token::Punctuator::PERIOD)) { // @todo wrap NON_UNIQUE->if->UNIQUE stuff
				UNIQUE

				auto m = std::make_shared<MemberExpression>();
				m->dereference = false;
				m->base = node;

				NON_OPTIONAL(read_identifier(m->id))

				node = m;
			} else if (read_punctuator(Token::Punctuator::ARROW)) {
				UNIQUE

				auto m = std::make_shared<MemberExpression>();
				m->dereference = true;
				m->base = node;

				NON_OPTIONAL(read_identifier(m->id))

				node = m;
			} else if (read_punctuator(Token::Punctuator::PLUSPLUS)) {
				auto p = std::make_shared<PostExpression>();
				p->op = Token::Punctuator::PLUSPLUS;
				p->base = node;

				node = p;
			} else if (read_punctuator(Token::Punctuator::MINUSMINUS)) {
				auto p = std::make_shared<PostExpression>();
				p->op = Token::Punctuator::MINUSMINUS;
				p->base = node;

				node = p;
			} else
				break;

			node->pos = pos;
		}
	END_OPTION

	bool read_unary_expression(Ptr<Expression> &node)
	OPTION
		auto unary_node = std::make_shared<UnaryExpression>();
		unary_node->op = peek().punctuator;
		unary_node->pos = peek().pos;

		BEGIN_UNIQUE(read_punctuator(Token::Punctuator::PLUSPLUS) || read_punctuator(Token::Punctuator::MINUSMINUS))
		NON_OPTIONAL(read_unary_expression(unary_node->operand))
		node = unary_node;
	ELSE_OPTION
		auto unary_node = std::make_shared<UnaryExpression>();
		unary_node->pos = peek().pos;

		BEGIN_UNIQUE(read_unary_operator(unary_node->op))
		NON_OPTIONAL(read_cast_expression(unary_node->operand))
		node = unary_node;
	ELSE_OPTION
		Ptr<Expression> u;
		auto pos = peek().pos;

		NON_UNIQUE
		NON_OPTIONAL(read_keyword(Token::Keyword::SIZEOF))

		if (read_unary_expression(u)) {
			auto s = std::make_shared<SizeofExpressionUnary>();
			s->expression = u;
			node = s;
		} else if (read_punctuator(Token::Punctuator::RB_OPEN)) {
			UNIQUE

			auto s = std::make_shared<SizeofExpressionTypeName>();
			NON_OPTIONAL(read_type_name(s->type))
			NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
			node = s;
		} else
			error("sizeof operand expected");

		node->pos = pos;
	ELSE_OPTION
		TypeName type_name;

		BEGIN_UNIQUE(read_keyword(Token::Keyword::_ALIGNOF))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
		NON_OPTIONAL(read_type_name(type_name))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
	ELSE_OPTION
		ALLOW_FAILURE(read_postfix_expression(node))
	OTHERWISE_FAIL("unary expression expected")

	bool read_cast_expression(Ptr<Expression> &node)
	OPTION
		auto c = std::make_shared<CastExpression>();
		c->pos = peek().pos;

		NON_UNIQUE
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
		NON_OPTIONAL(read_type_name(c->type))
		NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))

		NON_OPTIONAL(read_cast_expression(c->expression))

		node = c;
	ELSE_OPTION
		ALLOW_FAILURE(read_unary_expression(node))
	OTHERWISE_FAIL("cast expression expected")

	bool read_expression_with_precedence(Token::Precedence left_precedence, Ptr<Expression> &node)
	OPTION
		Ptr<Expression> lhs;
		BEGIN_UNIQUE(read_cast_expression(lhs))

		while (!eof()) {
			Token::Punctuator op = peek().punctuator;
			Token::Precedence right_precedence = Token::precedence(op);

			if ((op == Token::Punctuator::QMARK || Token::precedence(op) == Token::Precedence::ASSIGNMENT) ?
				(right_precedence > left_precedence) : // right-assoc
				(right_precedence >= left_precedence)  // left-assoc
				)
				break;

			if (peek().punctuator == Token::Punctuator::QMARK) {
				// conditional operator

				auto tree = std::make_shared<ConditionalExpression>();
				tree->condition = lhs;
				tree->pos = peek().pos;
				lhs = tree;

				shift();

				auto elist = std::make_shared<ExpressionList>();
				tree->when_true = elist;

				UNIQUE
				NON_OPTIONAL(read_expression(*elist))
				NON_OPTIONAL(read_punctuator(Token::Punctuator::COLON))
				NON_OPTIONAL(read_expression_with_precedence(Token::Precedence::CONDITIONAL, tree->when_false))
			} else {
				// ordinary operator

				if (Token::precedence(peek().punctuator) == Token::Precedence::ASSIGNMENT &&
					dynamic_cast<BinaryExpression *>(lhs.get())
					) {
					UNIQUE
					error("expression is not assignable");
				}

				auto tree = std::make_shared<BinaryExpression>();
				tree->op = peek().punctuator;
				tree->pos = peek().pos;
				tree->lhs = lhs;
				lhs = tree;

				shift();

				NON_OPTIONAL(read_expression_with_precedence(right_precedence, tree->rhs))
			}
		}

		node = lhs;
	OTHERWISE_FAIL("expression expected")

	bool read_assignment_expression(Ptr<Expression> &node)
	OPTION
		NON_OPTIONAL(read_expression_with_precedence(Token::Precedence::NONE, node))
	OTHERWISE_FAIL("assignment expression expected")

	bool read_constant_expression(Ptr<Expression> &node)
	OPTION
		NON_OPTIONAL(read_expression_with_precedence(Token::Precedence::ASSIGNMENT, node))
	END_OPTION
};

#undef _DEBUG_RETURN
#undef FAIL
#undef FAIL_IF_EMPTY
#undef _OPTION_PREFIX
#undef _OPTION_SUFFIX
#undef OPTION
#undef ELSE_OPTION
#undef END_OPTION
#undef ERROR
#undef OTHERWISE_FAIL
#undef ALLOW_FAILURE
#undef NON_OPTIONAL
#undef UNIQUE
#undef NON_UNIQUE
#undef BEGIN_UNIQUE
#undef OPTIONAL

#endif /* Parser_hpp */
