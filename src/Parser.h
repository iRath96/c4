#ifndef Parser_hpp
#define Parser_hpp

#include <stdio.h>
#include <iostream>
#include <vector>
#include <memory>
#include <sstream>

#include "Lexer.h"
#include "AST.h"

using namespace lexer;

extern bool debug_mode;

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

extern DebugTree dbg_tree_root;
extern DebugTree *dbg_tree_current;

#define _DEBUG_RETURN(x) { \
    if (debug_mode) \
        dbg_tree_current = dbg_tree_current->perform_return(x, i); \
    return x; \
}

#define FAIL goto deny;
#define FAIL_IF_EMPTY \
    if (i == _initial_i) \
        FAIL

#define _OPTION_PREFIX \
    { \
        __label__ deny;

#define _OPTION_SUFFIX \
        _DEBUG_RETURN(true) \
        deny: \
        i = _initial_i; \
        error_flag = _initial_ef; \
    }

#define OPTION { \
    if (debug_mode) { \
        /* printf("%d: %s\n", i, __PRETTY_FUNCTION__); */ \
        dbg_tree_current = dbg_tree_current->create_child(__PRETTY_FUNCTION__, i); \
    } \
    int _initial_i = i; \
    bool _initial_ef = error_flag; \
    _OPTION_PREFIX;

#define ELSE_OPTION  \
    _OPTION_SUFFIX \
    _OPTION_PREFIX

#define END_OPTION \
    _OPTION_SUFFIX \
    _DEBUG_RETURN(false) \
}

#define ERROR(error_message) if (error_flag) error(error_message);

#define OTHERWISE_FAIL(error_message) \
    _OPTION_SUFFIX \
    ERROR(error_message) \
    error_flag = _initial_ef; \
    _DEBUG_RETURN(false) \
}

#define ALLOW_FAILURE(stmt) \
    { \
        bool _prev_ef = error_flag; \
        error_flag = false; \
        if (!(stmt)) \
            FAIL \
        error_flag = _prev_ef; \
    }

#define NON_OPTIONAL(stmt) \
    if (!(stmt)) \
        FAIL;

#define UNIQUE \
    error_flag = true;

#define NON_UNIQUE \
    error_flag = false;

#define BEGIN_UNIQUE(stmt) \
    NON_UNIQUE \
    if (!(stmt)) \
        FAIL \
    else UNIQUE

#define OPTIONAL(stmt) \
    { \
        bool _prev_ef = error_flag; \
        error_flag = false; \
        stmt; \
        error_flag = _prev_ef; \
    }

const char *operator_name(Token::Punctuator punctuator);

class ParserError {
public:
    std::string message;
    TextPosition pos;
    
    ParserError(const std::string &message, TextPosition pos)
    : message(message), pos(pos) {}
};

class Parser {
public:
    ast::PtrVector<ast::ExternalDeclaration> declarations;
    
    Parser(Lexer lexer) : lexer(lexer) {
    }
    
    void parse() {
        read_list(&Parser::read_external_declaration, declarations);
        
        if (!eof() || declarations.empty()) error("declaration expected");
        
        //dbg_tree_root.dump(this);
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
    bool error_flag = true; // whether errors will be thrown
    
    bool shift(bool condition = true) {
        i += condition ? 1 : 0;
        return condition;
    }
    
    Lexer lexer;
    std::vector<Token> token_queue;
    
    bool eof(int offset = 0) {
        int i = this->i + offset;
        return (lexer.has_ended() && (int)token_queue.size() <= i) || peek(offset).kind == Token::Kind::END;
    }
    
    Token &peek(int offset = 0) {
        int i = this->i + offset;
        
        if (i < (int)token_queue.size())
            return token_queue[i];
        
        while ((int)token_queue.size() <= i) {
            // printf("reading token...\n");
            token_queue.push_back(lexer.next_token());
        }
        
        return token_queue[i];
    }
    
    [[noreturn]] void error(const std::string &message, int offset = 0) {
        TextPosition start_pos = peek(offset).pos;
        throw ParserError(message, start_pos);
    }
    
#pragma mark - Terminals
    
    bool read_punctuator(Token::Punctuator punctuator)
    OPTION
        NON_OPTIONAL(peek().punctuator == punctuator)
        shift();
    OTHERWISE_FAIL(std::string(operator_name(punctuator)) + " expected")
    
    bool read_keyword(Token::Keyword keyword)
    OPTION
        NON_OPTIONAL(peek().keyword == keyword)
        shift();
    OTHERWISE_FAIL("keyword (@todo) expected");
    
    bool read_unary_operator(Token::Punctuator &op)
    OPTION
        switch (peek().punctuator) {
            case Token::Punctuator::BIT_AND:
            case Token::Punctuator::ASTERISK:
            case Token::Punctuator::PLUS:
            case Token::Punctuator::MINUS:
            case Token::Punctuator::BIT_NOT:
            case Token::Punctuator::LOG_NOT:
                op = peek().punctuator;
                shift();
                break;
                
            default:
                FAIL
        }
    OTHERWISE_FAIL("unary operator expected")
    
    bool read_token(Token::Kind kind, std::string &text)
    OPTION
        NON_OPTIONAL(peek().kind == kind)
    
        text = peek().text;
        shift();
    END_OPTION
    
    bool read_identifier(std::string &text)
    OPTION
        NON_OPTIONAL(read_token(Token::Kind::IDENTIFIER, text))
    OTHERWISE_FAIL("identifier expected")
    
    bool read_constant(std::string &text)
    OPTION
        NON_OPTIONAL(read_token(Token::Kind::CONSTANT, text))
    OTHERWISE_FAIL("constant expected")
    
    bool read_string_literal(std::string &text)
    OPTION
        NON_OPTIONAL(read_token(Token::Kind::STRING_LITERAL, text))
    OTHERWISE_FAIL("string literal expected")
    
#pragma mark - Other stuff
    
    bool read_type_specifier_keyword(ast::NamedTypeSpecifier &node) {
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
    bool read_separated_list(bool (Parser::*method)(T &node), Token::Punctuator separator, std::vector<T> &result) {
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
            error_flag = true;
        }
        
        error_flag = _initial_ef;
        i = last_good_i;
        return i != initial_i;
    }
    
#pragma mark - Declarations
    
    bool read_declaration_specifiers(ast::PtrVector<ast::TypeSpecifier> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
    OTHERWISE_FAIL("type specifier expected")
    
    bool read_declarator_na(ast::Declarator &node) {
        return read_declarator(node, false);
    }
    
    bool read_declarator(ast::Declarator &node, bool isAbstract)
    OPTION
        node.pos = peek().pos;
    
        bool has_pointer;
        OPTIONAL(has_pointer = read_pointer(node.modifiers))
    
        if (has_pointer && isAbstract)
            OPTIONAL(read_direct_declarator(node, isAbstract))
        else
            NON_OPTIONAL(read_direct_declarator(node, isAbstract))
    END_OPTION
    
    bool read_direct_declarator_prefix(ast::Declarator &node, bool isAbstract)
    OPTION
        ALLOW_FAILURE(!isAbstract && read_identifier(node.name))
    ELSE_OPTION
        ALLOW_FAILURE(read_punctuator(Token::Punctuator::RB_OPEN))
        NON_OPTIONAL(read_declarator(node, isAbstract))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
    END_OPTION
    
    bool read_direct_declarator(ast::Declarator &node, bool isAbstract)
    OPTION
        int last_good_i;
    
        size_t insertionIndex = node.modifiers.size(); // @todo not efficient
        NON_OPTIONAL(read_direct_declarator_prefix(node, isAbstract))
    
        last_good_i = i;
        while (!eof()) {
            NON_UNIQUE
            auto p_suffix = std::make_shared<ast::DeclaratorParameterList>();
            if (!read_punctuator(Token::Punctuator::RB_OPEN)) break;
            
            UNIQUE
            OPTIONAL(read_parameter_type_list(p_suffix->parameters));
            
            if (p_suffix->parameters.size() == 1) {
                auto p = p_suffix->parameters.front();
                if (p.declarator.isAbstract() && p.declarator.modifiers.empty() && p.specifiers.size() == 1) {
                    auto s = p.specifiers.front();
                    if (auto nt = dynamic_cast<ast::NamedTypeSpecifier *>(s.get()))
                        if (nt->id == "void")
                            p_suffix->parameters.clear(); // no parameters taken
                }
            }
            
            if (!read_punctuator(Token::Punctuator::RB_CLOSE)) break;
            node.modifiers.insert(node.modifiers.begin() + insertionIndex, p_suffix);
            last_good_i = i;
        }
    
        i = last_good_i;
    OTHERWISE_FAIL("direct declarator expected")
    
    bool read_type_name(ast::TypeName &node)
    OPTION
        NON_OPTIONAL(read_specifier_qualifier_list(node.specifiers))
        OPTIONAL(read_declarator(node.declarator, true))
    END_OPTION
    
    bool read_parameter_type_list(ast::Vector<ast::ParameterDeclaration> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_parameter_declaration, Token::Punctuator::COMMA, node))
    END_OPTION
    
    bool read_parameter_declaration(ast::ParameterDeclaration &node)
    OPTION
        node.pos = peek().pos;
    
        NON_OPTIONAL(read_declaration_specifiers(node.specifiers))
        
        NON_UNIQUE
        if (read_declarator(node.declarator, false)) {
        } else {
            node.declarator = ast::Declarator(); // @fixme not elegant
            if (read_declarator(node.declarator, true)) {}
        }
    END_OPTION
    
    bool read_pointer_single(ast::Ptr<ast::DeclaratorModifier> &node)
    OPTION
        NON_OPTIONAL(read_punctuator(Token::Punctuator::ASTERISK))
        node = std::make_shared<ast::DeclaratorPointer>();
    END_OPTION
    
    bool read_pointer(ast::PtrVector<ast::DeclaratorModifier> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_pointer_single, node))
    END_OPTION
    
    bool read_specifier_qualifier_list(ast::PtrVector<ast::TypeSpecifier> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
    OTHERWISE_FAIL("specifier qualifier list expected")
    
#pragma mark - Structs
    
    bool read_struct_declarator_list(ast::Vector<ast::Declarator> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_declarator_na, Token::Punctuator::COMMA, node))
    END_OPTION
    
    bool read_struct_declaration(ast::Declaration &node)
    OPTION
        BEGIN_UNIQUE(read_specifier_qualifier_list(node.specifiers))
        OPTIONAL(read_struct_declarator_list(node.declarators))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
    END_OPTION
    
    bool read_struct_declaration_list(std::vector<ast::Declaration> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_struct_declaration, node))
    OTHERWISE_FAIL("struct declaration list expected")
    
    bool read_struct_body(ast::ComposedTypeSpecifier &node)
    OPTION
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_OPEN))
        NON_OPTIONAL(read_struct_declaration_list(node.declarations))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_CLOSE))
    END_OPTION
    
    bool read_type_specifier(ast::Ptr<ast::TypeSpecifier> &node)
    OPTION
        ast::NamedTypeSpecifier type_name;
        if (read_type_specifier_keyword(type_name)) {
            node = std::make_shared<ast::NamedTypeSpecifier>(type_name);
        } else {
            Token &token = peek();
            switch (token.keyword) {
                case Token::Keyword::STRUCT:
                case Token::Keyword::UNION: {
                    shift();
                    
                    auto n = new ast::ComposedTypeSpecifier();
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
    
    bool read_initializer(ast::Declarator &node)
    OPTION
        ast::Ptr<ast::Expression> assignment_expr;
        ALLOW_FAILURE(read_assignment_expression(assignment_expr))
        node.initializer = assignment_expr;
    OTHERWISE_FAIL("initializer expected")
    
    bool read_init_declarator(ast::Declarator &node)
    OPTION
        NON_OPTIONAL(read_declarator(node, false))
    
        NON_UNIQUE
        if (read_punctuator(Token::Punctuator::ASSIGN)) {
            UNIQUE
            NON_OPTIONAL(read_initializer(node));
        }
    END_OPTION
    
    bool read_init_declarator_list(ast::Vector<ast::Declarator> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_init_declarator, Token::Punctuator::COMMA, node))
    END_OPTION
    
    bool read_declaration(ast::Declaration &node)
    OPTION
        node.pos = peek().pos;
        
        BEGIN_UNIQUE(read_declaration_specifiers(node.specifiers))
        OPTIONAL(read_init_declarator_list(node.declarators))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
    END_OPTION
    
    bool read_declaration_list(std::vector<ast::Declaration> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_declaration, node))
    END_OPTION
    
    bool read_external_declaration(ast::Ptr<ast::ExternalDeclaration> &node)
    OPTION
        auto pos = peek().pos;
    
        ast::PtrVector<ast::TypeSpecifier> specifiers;
        ast::Declarator declarator;
        bool has_declarator, needs_declaration_list, needs_initialization, is_declaration;
        
        NON_OPTIONAL(read_declaration_specifiers(specifiers))
    
        NON_UNIQUE
        has_declarator = read_declarator(declarator, false);
        needs_declaration_list = has_declarator && peek().punctuator == Token::Punctuator::COMMA;
        needs_initialization = has_declarator && peek().punctuator == Token::Punctuator::ASSIGN;
        is_declaration = needs_initialization || needs_declaration_list || peek().punctuator == Token::Punctuator::SEMICOLON;
    
        UNIQUE
        if (is_declaration) {
            // declaration: ... (',' init-declarator-list(opt))(opt) ;
            
            auto n = new ast::ExternalDeclarationVariable();
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            
            if (needs_initialization) {
                shift(); // consume assign
                read_initializer(declarator);
                
                needs_declaration_list = peek().punctuator == Token::Punctuator::COMMA;
            }
            
            if (has_declarator)
                node->declarators.push_back(std::move(declarator));
            
            if (needs_declaration_list) {
                shift(); // jump over comma
                read_init_declarator_list(node->declarators);
            }
            
            NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
        } else {
            // function definition: ... declaration-list(opt) compound-statement
            
            if (!has_declarator)
                read_declarator(declarator, false);
            
            auto n = new ast::ExternalDeclarationFunction();
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            node->declarators.push_back(std::move(declarator));
            
            OPTIONAL(read_declaration_list(n->declarations))
            NON_OPTIONAL(read_compound_statement(n->body))
        }
    
        node->pos = pos;
    END_OPTION
    
#pragma mark - Statements
    
    bool read_statement(ast::Ptr<ast::Statement> &node)
    {
    auto pos = peek().pos;
    OPTION
        ALLOW_FAILURE(read_labeled_statement(node))
    ELSE_OPTION
        auto c = std::make_shared<ast::CompoundStatement>();
        ALLOW_FAILURE(read_compound_statement(*c))
        c->pos = pos;
        node = c;
    ELSE_OPTION
        auto e = std::make_shared<ast::ExpressionStatement>();
        ALLOW_FAILURE(read_expression_statement(*e))
        e->pos = pos;
        node = e;
    ELSE_OPTION
        ast::Ptr<ast::SelectionStatement> s;
        ALLOW_FAILURE(read_selection_statement(s))
        s->pos = pos;
        node = s;
    ELSE_OPTION
        ast::Ptr<ast::IterationStatement> stmt;
        ALLOW_FAILURE(read_iteration_statement(stmt))
        stmt->pos = pos;
        node = stmt;
    ELSE_OPTION
        ast::Ptr<ast::JumpStatement> j;
        ALLOW_FAILURE(read_jump_statement(j))
        j->pos = pos;
        node = j;
    OTHERWISE_FAIL("statement expected")
    }
    
    bool read_labeled_statement(ast::Ptr<ast::Statement> &node)
    OPTION
        ast::Ptr<ast::Label> label;
        auto pos = peek().pos;
    
        if (read_keyword(Token::Keyword::CASE)) {
            auto n = std::make_shared<ast::CaseLabel>();
            NON_OPTIONAL(read_constant_expression(n->expression))
            label = n;
        } else if (read_keyword(Token::Keyword::DEFAULT)) {
            label = std::make_shared<ast::DefaultLabel>();
        } else if (peek(0).kind == Token::Kind::IDENTIFIER) {
            auto n = std::make_shared<ast::IdentifierLabel>();
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
    
    bool read_block_item(ast::Ptr<ast::BlockItem> &node)
    OPTION
        auto n = std::make_shared<ast::Declaration>();
        NON_OPTIONAL(read_declaration(*n))
        node = n;
    ELSE_OPTION
        ast::Ptr<ast::Statement> stmt;
        NON_OPTIONAL(read_statement(stmt))
        node = stmt;
    END_OPTION
    
    bool read_block_item_list(ast::PtrVector<ast::BlockItem> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_block_item, node, Token::Punctuator::CB_CLOSE))
    END_OPTION
    
    bool read_compound_statement(ast::CompoundStatement &node)
    OPTION
        BEGIN_UNIQUE(read_punctuator(Token::Punctuator::CB_OPEN))
        OPTIONAL(read_block_item_list(node.items))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_CLOSE))
    OTHERWISE_FAIL("compound statement expected")
    
    bool read_expression_statement(ast::ExpressionStatement &node)
    OPTION
        bool has_expression = read_expression(node.expressions);
        error_flag = error_flag || has_expression;
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
    END_OPTION
    
    bool read_selection_statement(ast::Ptr<ast::SelectionStatement> &node)
    OPTION
        BEGIN_UNIQUE(read_keyword(Token::Keyword::IF))
    
        node = std::make_shared<ast::SelectionStatement>();
    
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
    
    bool read_iteration_statement(ast::Ptr<ast::IterationStatement> &node)
    OPTION
        BEGIN_UNIQUE(read_keyword(Token::Keyword::WHILE))
        
        node = std::make_shared<ast::IterationStatement>();
        
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
        NON_OPTIONAL(read_expression(node->condition))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
        NON_OPTIONAL(read_statement(node->body))
    END_OPTION
    
    bool read_jump_statement(ast::Ptr<ast::JumpStatement> &node)
    OPTION
        auto g = std::make_shared<ast::GotoStatement>();
    
        BEGIN_UNIQUE(read_keyword(Token::Keyword::GOTO))
        NON_OPTIONAL(read_identifier(g->target))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
    
        node = g;
    ELSE_OPTION
        Token::Keyword keyword = peek().keyword;
        ast::Ptr<ast::ContinueStatement> c;
    
        BEGIN_UNIQUE(read_keyword(Token::Keyword::CONTINUE) || read_keyword(Token::Keyword::BREAK))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
    
        c = std::make_shared<ast::ContinueStatement>();
        c->keyword = keyword;
        node = c;
    ELSE_OPTION
        auto r = std::make_shared<ast::ReturnStatement>();
    
        BEGIN_UNIQUE(read_keyword(Token::Keyword::RETURN))
        OPTIONAL(read_expression(r->expressions))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
    
        node = r;
    END_OPTION
    
#pragma mark - Expressions
    
    bool read_expression(ast::ExpressionList &node)
    OPTION
        node.pos = peek().pos;
        NON_OPTIONAL(read_separated_list(&Parser::read_assignment_expression, Token::Punctuator::COMMA, node.children))
    END_OPTION
    
    bool read_primary_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        auto constant = std::make_shared<ast::ConstantExpression>();
        constant->pos = peek().pos;
        constant->isIdentifier = read_identifier(constant->text);
    
        NON_OPTIONAL(constant->isIdentifier || read_constant(constant->text) || read_string_literal(constant->text))
        node = constant;
    ELSE_OPTION
        auto list = std::make_shared<ast::ExpressionList>();
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
        NON_OPTIONAL(read_expression(*list))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
        node = list;
    END_OPTION
    
    bool read_argument_expression_list(ast::PtrVector<ast::Expression> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_assignment_expression, Token::Punctuator::COMMA, node))
    END_OPTION
    
    bool read_postfix_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        NON_OPTIONAL(read_primary_expression(node))
        
        while (!eof()) {
            TextPosition pos = peek().pos;
            
            NON_UNIQUE
            if (read_punctuator(Token::Punctuator::SB_OPEN)) {
                UNIQUE
                
                auto s = std::make_shared<ast::SubscriptExpression>();
                s->base = node;
                
                NON_OPTIONAL(read_expression(s->subscript))
                NON_OPTIONAL(read_punctuator(Token::Punctuator::SB_CLOSE))
                
                node = s;
            } else if (read_punctuator(Token::Punctuator::RB_OPEN)) {
                UNIQUE
                
                auto n = std::make_shared<ast::CallExpression>();
                n->function = node;
                
                OPTIONAL(read_argument_expression_list(n->arguments))
                NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
                
                node = n;
            } else if (read_punctuator(Token::Punctuator::PERIOD)) { // @todo wrap NON_UNIQUE->if->UNIQUE stuff
                UNIQUE
                
                auto m = std::make_shared<ast::MemberExpression>();
                m->dereference = false;
                m->base = node;
                
                NON_OPTIONAL(read_identifier(m->id))
                
                node = m;
            } else if (read_punctuator(Token::Punctuator::ARROW)) {
                UNIQUE
                
                auto m = std::make_shared<ast::MemberExpression>();
                m->dereference = true;
                m->base = node;
                
                NON_OPTIONAL(read_identifier(m->id))
                
                node = m;
            } else if (read_punctuator(Token::Punctuator::PLUSPLUS)) {
                auto p = std::make_shared<ast::PostExpression>();
                p->op = Token::Punctuator::PLUSPLUS;
                p->base = node;
                
                node = p;
            } else if (read_punctuator(Token::Punctuator::MINUSMINUS)) {
                auto p = std::make_shared<ast::PostExpression>();
                p->op = Token::Punctuator::MINUSMINUS;
                p->base = node;
                
                node = p;
            } else
                break;
            
            node->pos = pos;
        }
    END_OPTION
    
    bool read_unary_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        auto unary_node = std::make_shared<ast::UnaryExpression>();
        unary_node->op = peek().punctuator;
        unary_node->pos = peek().pos;
    
        BEGIN_UNIQUE(read_punctuator(Token::Punctuator::PLUSPLUS) || read_punctuator(Token::Punctuator::MINUSMINUS))
        NON_OPTIONAL(read_unary_expression(unary_node->operand))
        node = unary_node;
    ELSE_OPTION
        auto unary_node = std::make_shared<ast::UnaryExpression>();
        unary_node->pos = peek().pos;
    
        BEGIN_UNIQUE(read_unary_operator(unary_node->op))
        NON_OPTIONAL(read_cast_expression(unary_node->operand))
        node = unary_node;
    ELSE_OPTION
        ast::Ptr<ast::Expression> u;
        TextPosition pos = peek().pos;
    
        NON_UNIQUE
        NON_OPTIONAL(read_keyword(Token::Keyword::SIZEOF))
    
        if (read_unary_expression(u)) {
            auto s = std::make_shared<ast::SizeofExpressionUnary>();
            s->expression = u;
            node = s;
        } else if (read_punctuator(Token::Punctuator::RB_OPEN)) {
            UNIQUE
            
            auto s = std::make_shared<ast::SizeofExpressionTypeName>();
            NON_OPTIONAL(read_type_name(s->type))
            NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
            node = s;
        } else
            error("sizeof operand expected");
        
        node->pos = pos;
    ELSE_OPTION
        ast::TypeName type_name;
    
        BEGIN_UNIQUE(read_keyword(Token::Keyword::_ALIGNOF))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
        NON_OPTIONAL(read_type_name(type_name))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
    ELSE_OPTION
        ALLOW_FAILURE(read_postfix_expression(node))
    OTHERWISE_FAIL("unary expression expected")
    
    bool read_cast_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        auto c = std::make_shared<ast::CastExpression>();
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
    
    bool read_expression_with_precedence(Token::Precedence left_precedence, ast::Ptr<ast::Expression> &node)
    OPTION
        ast::Ptr<ast::Expression> root; // @todo rename this to lhs
        BEGIN_UNIQUE(read_cast_expression(root))
    
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
                
                auto tree = std::make_shared<ast::ConditionalExpression>();
                tree->condition = root;
                tree->pos = peek().pos;
                root = tree;
                
                shift();
                
                auto elist = std::make_shared<ast::ExpressionList>();
                tree->when_true = elist;
                
                UNIQUE
                NON_OPTIONAL(read_expression(*elist))
                NON_OPTIONAL(read_punctuator(Token::Punctuator::COLON))
                NON_OPTIONAL(read_expression_with_precedence(Token::Precedence::CONDITIONAL, tree->when_false))
            } else {
                // ordinary operator
                
                if (Token::precedence(peek().punctuator) == Token::Precedence::ASSIGNMENT &&
                    dynamic_cast<ast::BinaryExpression *>(root.get())
                    ) {
                    UNIQUE
                    error("expression is not assignable");
                }
                
                auto tree = std::make_shared<ast::BinaryExpression>();
                tree->op = peek().punctuator;
                tree->pos = peek().pos;
                tree->lhs = root;
                root = tree;
                
                shift();
                
                NON_OPTIONAL(read_expression_with_precedence(right_precedence, tree->rhs))
            }
        }
        
        node = root;
    OTHERWISE_FAIL("expression expected")
    
    bool read_assignment_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        NON_OPTIONAL(read_expression_with_precedence(Token::Precedence::NONE, node))
    OTHERWISE_FAIL("assignment expression expected")
    
    bool read_constant_expression(ast::Ptr<ast::Expression> &node)
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
