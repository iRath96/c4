//
//  Parser.hpp
//  c4
//
//  Created by Alexander Rath on 04.11.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

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

#define DEBUG_HOOK { \
    if (debug_mode) { \
        /* printf("%d: %s\n", i, __PRETTY_FUNCTION__); */ \
        dbg_tree_current = dbg_tree_current->create_child(__PRETTY_FUNCTION__, i); \
    } \
}

#define DEBUG_RETURN(x) { \
    if (debug_mode) \
        dbg_tree_current = dbg_tree_current->perform_return(x, i); \
    return x; \
}

#define ACCEPT DEBUG_RETURN(true)
#define DENY DEBUG_RETURN(false)

#define NON_EMPTY(stmt, err) {\
    if (!(stmt)) \
        error(err); \
}

#define NON_EMPTY_RET(stmt) {\
    if (!(stmt)) \
        DEBUG_RETURN(false); \
}

#define _OPTION_PREFIX \
    { \
        __label__ deny;

#define _OPTION_SUFFIX \
        ACCEPT \
        deny: \
        i = _initial_i; \
        error_flag = _initial_ef; \
    }

#define OPTION { \
    DEBUG_HOOK \
    int _initial_i = i; \
    bool _initial_ef = error_flag; \
    _OPTION_PREFIX;

#define ELSE_OPTION  \
    _OPTION_SUFFIX \
    _OPTION_PREFIX

#define END_OPTION \
    _OPTION_SUFFIX \
    DENY \
}

#define ERROR(error_message) if (error_flag) error(error_message);

#define OTHERWISE_FAIL(error_message) \
    _OPTION_SUFFIX \
    ERROR(error_message) \
    error_flag = _initial_ef; \
    DENY \
}

#define ALLOW_FAILURE(stmt) \
    { \
        bool _prev_ef = error_flag; \
        error_flag = false; \
        if (!(stmt)) { \
            goto deny; \
        } \
        error_flag = _prev_ef; \
    }

#define NON_OPTIONAL(stmt) \
    if (!(stmt)) goto deny;

#define UNIQUE \
    error_flag = true;

#define NON_UNIQUE \
    error_flag = false;

#define BEGIN_UNIQUE(stmt) \
    if (!(stmt)) goto deny; \
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
    : message(message), pos(pos) {
        
    }
};

class Parser {
public:
    ast::PtrVector<ast::ExternalDeclaration> declarations;
    
    Parser(Lexer lexer) : lexer(lexer) {
    }
    
    void parse() {
        read_list(&Parser::read_external_declaration, declarations);
        
        if (!eof() || declarations.empty())
            error("declaration expected");
        
        //dbg_tree_root.dump(this);
    }
    
    void print_context(int index = -1) {
        if (index < 0)
            index = i;
        
        for (int j = -(index >= 5 ? 5 : index); j <= 5; ++j) {
            if (!j)
                std::cout << "(here) ";
            std::cout << peek(index - i + j).text << " ";
        }
        
        std::cout << std::endl;
    }
    
    void print_debug_tree() {
        dbg_tree_root.dump(this);
    }
    
protected:
    int i = 0;
    bool error_flag = false; // whtether errors will be thrown
    
    bool shift(bool condition = true) {
        i += condition ? 1 : 0;
        return condition;
    }
    
    Lexer lexer;
    std::vector<Token> token_queue;
    
    bool eof(int offset = 0) {
        int i = this->i + offset;
        return (lexer.has_ended() && (int)token_queue.size() <= i) || peek(offset).type == Token::Type::END;
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
        TextPosition start_pos = i + offset > 0 ?
            peek(offset - 1).end_pos :
            peek(offset).pos
        ;
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
                NON_OPTIONAL(false)
        }
    OTHERWISE_FAIL("unary operator expected")
    
    bool read_token(Token::Type type, const char *&text)
    OPTION
        NON_OPTIONAL(peek().type == type)
    
        text = peek().text;
        shift();
    END_OPTION
    
    bool read_identifier(const char *&text)
    OPTION
        NON_OPTIONAL(read_token(Token::Type::IDENTIFIER, text))
    OTHERWISE_FAIL("identifier expected")
    
    bool read_constant(const char *&text)
    OPTION
        NON_OPTIONAL(read_token(Token::Type::CONSTANT, text))
    OTHERWISE_FAIL("constant expected")
    
    bool read_string_literal(const char *&text)
    OPTION
        NON_OPTIONAL(read_token(Token::Type::STRING_LITERAL, text))
    OTHERWISE_FAIL("string literal expected")
    
#pragma mark - Other stuff
    
    bool read_type_specifier_keyword(ast::NamedType &node) {
        switch (peek().keyword) {
            case Token::Keyword::VOID:
            case Token::Keyword::CHAR:
            case Token::Keyword::SHORT:
            case Token::Keyword::INT:
            case Token::Keyword::LONG:
            case Token::Keyword::FLOAT:
            case Token::Keyword::DOUBLE:
            case Token::Keyword::SIGNED:
            case Token::Keyword::UNSIGNED:
            case Token::Keyword::_BOOL:
            case Token::Keyword::_COMPLEX:
                node.id = peek().text;
                return shift();
                
            default:
                return false;
        }
    }
    
    template<typename T>
    bool read_list(bool (Parser::*method)(T &node), std::vector<T> &result) {
        int initial_i = i;
        bool _initial_ef = error_flag;
        
        while (!eof()) {
            T temp;
            if (!(this->*method)(temp))
                break;
            
            error_flag = false;
            
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
            if (!(this->*method)(temp))
                break;
            
            result.push_back(temp);
            last_good_i = i;
            
            error_flag = false;
            
            if (!read_punctuator(separator))
                break;
        }
        
        error_flag = _initial_ef;
        i = last_good_i;
        return i != initial_i;
    }
    
#pragma mark - Declarations
    
    bool read_declarator(ast::Declarator &node)
    OPTION
        if (read_pointer(node)) {
            NON_OPTIONAL(read_direct_declarator(node))
        } else {
            OPTIONAL(read_direct_declarator(node))
        }
    END_OPTION
    
    bool read_declaration_specifiers(ast::PtrVector<ast::TypeSpecifier> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
    END_OPTION
    
    bool read_direct_declarator_prefix(ast::Declarator &node)
    OPTION
        NON_OPTIONAL(read_identifier(node.name))
    ELSE_OPTION
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
        NON_OPTIONAL(read_declarator(node))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
    
        node.is_function = true;
    END_OPTION
    
    bool read_direct_declarator(ast::Declarator &node) {
        DEBUG_HOOK
        
        NON_EMPTY_RET(read_direct_declarator_prefix(node));
        
        int last_good_i = i;
        
        while (!eof()) {
            // try reading parameter-list / identifier-list
            
            auto p_suffix = std::make_shared<ast::DeclaratorParameterList>();
            std::vector<const char *> identifier_list;
            
            if (!read_punctuator(Token::Punctuator::RB_OPEN))
                break;
            
            if (read_parameter_type_list(p_suffix->parameters)) {
                node.suffixes.push_back(p_suffix);
            } else if (read_identifier_list(identifier_list)) {
            }
            
            UNIQUE
            if (!read_punctuator(Token::Punctuator::RB_CLOSE))
                break;
            NON_UNIQUE
            
            last_good_i = i;
        }
        
        i = last_good_i;
        ACCEPT
    }
    
    bool read_identifier_list(std::vector<const char *> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_identifier, Token::Punctuator::COMMA, node))
    END_OPTION
    
    bool read_type_name(ast::TypeName &node)
    OPTION
        NON_OPTIONAL(read_specifier_qualifier_list(node.specifiers))
        OPTIONAL(read_abstract_declarator())
    END_OPTION
    
    bool read_abstract_declarator()
    OPTION
        ast::Declarator ptr;
    
        if (read_pointer(ptr))
            OPTIONAL(read_direct_abstract_declarator())
        else
            NON_OPTIONAL(read_direct_abstract_declarator())
    END_OPTION
    
    bool read_direct_abstract_declarator_prefix()
    OPTION
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
        NON_OPTIONAL(read_abstract_declarator())
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
    END_OPTION
    
    bool read_direct_abstract_declarator() {
        DEBUG_HOOK
        
        int initial_i = i;
        
        read_direct_abstract_declarator_prefix();
        
        while (!eof()) {
            ast::Vector<ast::ParameterDeclaration> plist;
            
            if (read_punctuator(Token::Punctuator::SB_OPEN)) {
                NON_EMPTY(read_punctuator(Token::Punctuator::ASTERISK), "* expected");
                NON_EMPTY(read_punctuator(Token::Punctuator::SB_CLOSE), "] expected");
            } else if (read_punctuator(Token::Punctuator::RB_OPEN)) {
                read_parameter_type_list(plist);
                NON_EMPTY(read_punctuator(Token::Punctuator::RB_CLOSE), ") expected");
            } else
                break;
        }
        
        DEBUG_RETURN(i != initial_i)
    }
    
    bool read_parameter_type_list(ast::Vector<ast::ParameterDeclaration> &node)
    OPTION
        NON_OPTIONAL(read_parameter_list(node))
    END_OPTION
    
    bool read_parameter_list(ast::Vector<ast::ParameterDeclaration> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_parameter_declaration, Token::Punctuator::COMMA, node))
    END_OPTION
    
    bool read_parameter_declaration(ast::ParameterDeclaration &node)
    OPTION
        NON_OPTIONAL(read_declaration_specifiers(node.specifiers))
        if (read_declarator(node.declarator)) {
        } else if (read_abstract_declarator()) {
        }
    END_OPTION
    
    bool read_pointer_single(ast::Pointer &node)
    OPTION
        NON_OPTIONAL(read_punctuator(Token::Punctuator::ASTERISK))
        OPTIONAL(read_type_qualifier_list())
    END_OPTION
    
    bool read_pointer(ast::Declarator &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_pointer_single, node.pointers))
    END_OPTION
    
    bool read_type_qualifier_list()
    OPTION
        ast::Vector<int> node;
        NON_OPTIONAL(read_list(&Parser::read_type_qualifier, node))
    END_OPTION
    
    bool read_specifier_qualifier_list(ast::PtrVector<ast::TypeSpecifier> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_type_specifier, node))
    END_OPTION
    
#pragma mark - Structs
    
    bool read_struct_declarator(ast::Declarator &node)
    OPTION
        NON_OPTIONAL(read_declarator(node))
    END_OPTION
    
    bool read_struct_declarator_list(std::vector<ast::Declarator> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_struct_declarator, Token::Punctuator::COMMA, node))
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
    
    bool read_struct_body(ast::ComposedType &node)
    OPTION
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_OPEN))
        NON_OPTIONAL(read_struct_declaration_list(node.declarations))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_CLOSE))
    OTHERWISE_FAIL("struct body expected")
    
    bool read_type_qualifier(int &node)
    OPTION
        switch (peek().keyword) {
            case Token::Keyword::CONST:
            case Token::Keyword::RESTRICT:
            case Token::Keyword::VOLATILE:
            case Token::Keyword::_ATOMIC:
                shift();
                break;
            
            default:
                DENY
        }
    END_OPTION
    
    bool read_type_specifier(ast::Ptr<ast::TypeSpecifier> &node) {
        DEBUG_HOOK
        
        ast::NamedType type_name;
        if (read_type_specifier_keyword(type_name)) {
            node = std::make_shared<ast::NamedType>(type_name);
            ACCEPT
        }
        
        Token &token = peek();
        switch (token.keyword) {
            case Token::Keyword::STRUCT:
            case Token::Keyword::UNION: {
                shift();
                
                auto n = new ast::ComposedType();
                node.reset(n);
                
                n->type = token.keyword;
                
                NON_UNIQUE
                
                bool has_identifier = read_identifier(n->name);
                bool has_body = peek().punctuator == Token::Punctuator::CB_OPEN;
                
                if (!has_body && !has_identifier)
                    error("struct/union without identifier or body");
                
                if (has_body) {
                    UNIQUE
                    NON_EMPTY_RET(read_struct_body(*n))
                }
                
                break;
            }
                
            default:
                DENY
        }
        
        ACCEPT
    }
    
    bool read_initializer(ast::Declarator &node)
    OPTION
        auto initializer_list = std::make_shared<ast::InitializerList>();
    
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_OPEN))
        NON_OPTIONAL(read_initializer_list(*initializer_list))
        OPTIONAL(read_punctuator(Token::Punctuator::COMMA))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_CLOSE))
    
        node.initializer = initializer_list;
    ELSE_OPTION
        ast::Ptr<ast::Expression> assignment_expr;
    
        NON_OPTIONAL(read_assignment_expression(assignment_expr))
    
        node.initializer = assignment_expr;
    END_OPTION
    
    bool read_designation_initializer_pair(ast::Initializer &node)
    OPTION
        if (read_designation(node.designators)) {
            NON_OPTIONAL(read_initializer(node.declarator))
        } else if (read_initializer(node.declarator)) {
        } else
            DENY
    END_OPTION
    
    bool read_initializer_list(ast::InitializerList &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_designation_initializer_pair, Token::Punctuator::COMMA, node.initializers))
    END_OPTION
    
    bool read_designation(ast::PtrVector<ast::Designator> &node)
    OPTION
        NON_OPTIONAL(read_designator_list(node))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::ASSIGN))
    END_OPTION
    
    bool read_designator_list(ast::PtrVector<ast::Designator> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_designator, node))
    END_OPTION
    
    bool read_designator(ast::Ptr<ast::Designator> &node)
    OPTION
        auto d = std::make_shared<ast::DesignatorWithExpression>();
    
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SB_OPEN))
        NON_OPTIONAL(read_constant_expression(d->expression))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SB_CLOSE))
    
        node = d;
    ELSE_OPTION
        auto d = std::make_shared<ast::DesignatorWithIdentifier>();
    
        NON_OPTIONAL(read_punctuator(Token::Punctuator::PERIOD))
        NON_OPTIONAL(read_identifier(d->id))
    
        node = d;
    END_OPTION
    
    bool read_init_declarator(ast::Declarator &node)
    OPTION
        NON_OPTIONAL(read_declarator(node))
        
        if (read_punctuator(Token::Punctuator::ASSIGN)) {
            // also read initializer
            NON_OPTIONAL(read_initializer(node));
        }
    END_OPTION
    
    bool read_init_declarator_list(std::vector<ast::Declarator> &node)
    OPTION
        NON_OPTIONAL(read_separated_list(&Parser::read_init_declarator, Token::Punctuator::COMMA, node))
    END_OPTION
    
    bool read_declaration(ast::Declaration &node)
    OPTION
        BEGIN_UNIQUE(read_declaration_specifiers(node.specifiers))
        OPTIONAL(read_init_declarator_list(node.declarators))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::SEMICOLON))
    END_OPTION
    
    bool read_declaration_list(std::vector<ast::Declaration> &node)
    OPTION
        NON_OPTIONAL(read_list(&Parser::read_declaration, node))
    END_OPTION
    
    bool read_external_declaration(ast::Ptr<ast::ExternalDeclaration> &node) {
        DEBUG_HOOK
        
        ast::PtrVector<ast::TypeSpecifier> specifiers;
        ast::Declarator declarator;
        
        NON_EMPTY_RET(read_declaration_specifiers(specifiers));
        
        bool has_declarator = read_declarator(declarator);
        bool needs_declaration_list = has_declarator && peek().punctuator == Token::Punctuator::COMMA;
        bool needs_initialization = has_declarator && peek().punctuator == Token::Punctuator::ASSIGN;
        bool is_declaration = needs_initialization || needs_declaration_list || peek().punctuator == Token::Punctuator::SEMICOLON;
        
        if (is_declaration) {
            // declaration: ... (',' init-declarator-list(opt))(opt) ;
            
            auto n = new ast::ExternalDeclarationVariable();
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            
            if (needs_initialization) {
                shift(); // consume assign
                read_initializer(declarator);
                
                needs_initialization = peek().punctuator == Token::Punctuator::COMMA;
            }
            
            node->declarators.push_back(std::move(declarator));
            
            if (needs_declaration_list) {
                shift(); // jump over comma
                read_init_declarator_list(node->declarators);
            }
            
            NON_EMPTY(read_punctuator(Token::Punctuator::SEMICOLON), "semicolon expected");
        } else {
            // function definition: ... declaration-list(opt) compound-statement
            
            auto n = new ast::ExternalDeclarationFunction();
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            node->declarators.push_back(std::move(declarator));
            
            read_declaration_list(n->declarations);
            NON_EMPTY(read_compound_statement(n->body), "compound statement expected");
        }
        
        ACCEPT
    }
    
#pragma mark - Statements
    
    bool read_statement(ast::Ptr<ast::Statement> &node)
    OPTION
        ALLOW_FAILURE(read_labeled_statement(node))
    ELSE_OPTION
        auto c = std::make_shared<ast::CompoundStatement>();
        ALLOW_FAILURE(read_compound_statement(*c))
        node = c;
    ELSE_OPTION
        auto e = std::make_shared<ast::ExpressionStatement>();
        ALLOW_FAILURE(read_expression_statement(*e))
        node = e;
    ELSE_OPTION
        ast::Ptr<ast::SelectionStatement> s;
        ALLOW_FAILURE(read_selection_statement(s))
        node = s;
    ELSE_OPTION
        ast::Ptr<ast::IterationStatement> stmt;
        ALLOW_FAILURE(read_iteration_statement(stmt))
        node = stmt;
    ELSE_OPTION
        ast::Ptr<ast::JumpStatement> j;
        ALLOW_FAILURE(read_jump_statement(j))
        node = j;
    OTHERWISE_FAIL("statement expected")
    
    bool read_labeled_statement(ast::Ptr<ast::Statement> &node)
    OPTION
        ast::Ptr<ast::Label> label;
    
        if (read_keyword(Token::Keyword::CASE)) {
            auto n = std::make_shared<ast::CaseLabel>();
            NON_OPTIONAL(read_constant_expression(n->expression))
            label = n;
        } else if (read_keyword(Token::Keyword::DEFAULT)) {
            label = std::make_shared<ast::DefaultLabel>();
        } else if (peek(0).type == Token::Type::IDENTIFIER) {
            auto n = std::make_shared<ast::IdentifierLabel>();
            read_identifier(n->id);
            label = n;
        } else
            DENY
        
        NON_OPTIONAL(read_punctuator(Token::Punctuator::COLON))
        
        UNIQUE
        NON_OPTIONAL(read_statement(node))
        
        node->labels.push_back(label);
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
        NON_OPTIONAL(read_list(&Parser::read_block_item, node))
    END_OPTION
    
    bool read_compound_statement(ast::CompoundStatement &node)
    OPTION
        BEGIN_UNIQUE(read_punctuator(Token::Punctuator::CB_OPEN))
        OPTIONAL(read_block_item_list(node.items))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_CLOSE))
    END_OPTION
    
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
        NON_OPTIONAL(read_separated_list(&Parser::read_assignment_expression, Token::Punctuator::COMMA, node.children))
    END_OPTION
    
    bool read_primary_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        auto constant = std::make_shared<ast::ConstantExpression>();
        NON_OPTIONAL(read_identifier(constant->text) || read_constant(constant->text) || read_string_literal(constant->text))
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
    
    bool read_postfix_expression_prefix(ast::Ptr<ast::Expression> &node)
    OPTION
        NON_OPTIONAL(read_primary_expression(node))
    ELSE_OPTION
        auto initializer = std::make_shared<ast::InitializerExpression>();

        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
    
        NON_OPTIONAL(read_type_name(initializer->type))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))

        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_OPEN))
        NON_OPTIONAL(read_initializer_list(initializer->initializers))
        OPTIONAL(read_punctuator(Token::Punctuator::COMMA))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::CB_CLOSE))
    
        node = initializer;
    END_OPTION
    
    bool read_postfix_expression(ast::Ptr<ast::Expression> &node) {
        DEBUG_HOOK
        
        NON_EMPTY_RET(read_postfix_expression_prefix(node))
        
        while (!eof()) {
            if (read_punctuator(Token::Punctuator::SB_OPEN)) {
                ast::ExpressionList list;
                NON_EMPTY(read_expression(list), "expression expected");
                NON_EMPTY(read_punctuator(Token::Punctuator::SB_CLOSE), "] expected");
            } else if (read_punctuator(Token::Punctuator::RB_OPEN)) {
                auto n = std::make_shared<ast::CallExpression>();
                n->function = node;
                
                read_argument_expression_list(n->arguments);
                NON_EMPTY(read_punctuator(Token::Punctuator::RB_CLOSE), ") expected");
                
                node = n;
            } else if (read_punctuator(Token::Punctuator::PERIOD)) {
                const char *id;
                NON_EMPTY(read_identifier(id), "subscript expected");
            } else if (read_punctuator(Token::Punctuator::ARROW)) {
                const char *id;
                NON_EMPTY(read_identifier(id), "subscript expected");
            } else if (read_punctuator(Token::Punctuator::PLUSPLUS)) {
            } else if (read_punctuator(Token::Punctuator::MINUSMINUS)) {
            } else
                break;
        }
        
        ACCEPT
    }
    
    bool read_unary_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        auto unary_node = std::make_shared<ast::UnaryExpression>();
        ALLOW_FAILURE(read_punctuator(Token::Punctuator::PLUSPLUS) || read_punctuator(Token::Punctuator::MINUSMINUS))
        UNIQUE
        NON_OPTIONAL(read_unary_expression(unary_node->operand))
        node = unary_node;
    ELSE_OPTION
        auto unary_node = std::make_shared<ast::UnaryExpression>();
        ALLOW_FAILURE(read_unary_operator(unary_node->op))
        UNIQUE
        NON_OPTIONAL(read_cast_expression(unary_node->operand))
        node = unary_node;
    ELSE_OPTION
        ast::Ptr<ast::Expression> u;
    
        ALLOW_FAILURE(read_keyword(Token::Keyword::SIZEOF))
        NON_UNIQUE
    
        if (read_unary_expression(u)) {
            auto s = std::make_shared<ast::SizeofExpressionUnary>();
            s->expression = u;
            node = s;
        } else if (read_punctuator(Token::Punctuator::RB_OPEN)) {
            auto s = std::make_shared<ast::SizeofExpressionTypeName>();
            NON_OPTIONAL(read_type_name(s->type))
            NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
            node = s;
        } else
            error("sizeof operand expected");
    ELSE_OPTION
        ast::TypeName type_name;
    
        ALLOW_FAILURE(read_keyword(Token::Keyword::_ALIGNOF))
        UNIQUE
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_OPEN))
        NON_OPTIONAL(read_type_name(type_name))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
    ELSE_OPTION
        ALLOW_FAILURE(read_postfix_expression(node))
    OTHERWISE_FAIL("unary expression expected")
    
    bool read_cast_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        ast::TypeName type_name;
    
        ALLOW_FAILURE(read_punctuator(Token::Punctuator::RB_OPEN))
        NON_OPTIONAL(read_type_name(type_name))
        NON_OPTIONAL(read_punctuator(Token::Punctuator::RB_CLOSE))
    
        NON_OPTIONAL(read_cast_expression(node))
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
                root = tree;
                
                shift();
                
                auto elist = std::make_shared<ast::ExpressionList>();
                tree->when_true = elist;
                
                NON_EMPTY(read_expression(*elist), "expression expected");
                NON_EMPTY(read_punctuator(Token::Punctuator::COLON), "colon expected");
                NON_EMPTY(read_expression_with_precedence((Token::Precedence)((int)right_precedence - 1), tree->when_false), "expression expected");
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
                tree->lhs = root;
                root = tree;
                
                shift();
                
                NON_EMPTY(read_expression_with_precedence(right_precedence, tree->rhs), "expression expected");
            }
        }
        
        node = root;
    END_OPTION
    
    bool read_assignment_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        NON_OPTIONAL(read_expression_with_precedence(Token::Precedence::NONE, node))
    END_OPTION
    
    bool read_constant_expression(ast::Ptr<ast::Expression> &node)
    OPTION
        NON_OPTIONAL(read_expression_with_precedence(Token::Precedence::ASSIGNMENT, node))
    END_OPTION
};

#undef NON_EMPTY
#undef NON_EMPTY_RET

#endif /* Parser_hpp */

