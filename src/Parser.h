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

#include "Lexer.h"

#define NON_EMPTY(stmt, err) {\
    if (!stmt) \
        error(err, i); \
}

#define NON_EMPTY_RET(stmt) {\
    if (!stmt) \
        return false; \
}

class ParserError {
public:
    std::string message;
    TextPosition pos;
    
    ParserError(const std::string &message, TextPosition pos)
    : message(message), pos(pos) {
        
    }
};

class Node {
public:
    virtual void dump(const std::string &indent) {
        std::cout << indent << "not yet implemented" << std::endl;
    }
};

class NodeIdentifier : public Node {
public:
    const char *id;
    NodeIdentifier(const char *id) : id(id) {}
};

class NodeTypeSpecifier : public Node {
public:
};

class NodeTypeNamed : public NodeTypeSpecifier {
public:
    const char *name;
    NodeTypeNamed(const char *name) : name(name) {}
};

class NodePointer : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
    
    virtual void dump(const std::string &indent) {
        std::cout << indent << "Pointer" << std::endl;
    }
};

class NodeDeclarator : public Node {
public:
    const char *name;
    std::vector<NodePointer> pointers;
};

class NodeDeclaration : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
    std::vector<NodeDeclarator> declarators;
};

class NodeExternalDeclaration : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
    std::vector<NodeDeclarator> declarators;
};

class NodeExternalDeclarationVariable : public NodeExternalDeclaration {
public:
};

class NodeExternalDeclarationFunction : public NodeExternalDeclaration {
public:
    std::vector<NodeDeclaration> declarations;
};

class NodeParameterDeclaration : public Node {
public:
    std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
    NodeDeclarator declarator;
};

class NodeTypeComposed : public NodeTypeSpecifier {
public:
    TokenKeyword type;
    std::vector<NodeDeclaration> declarations;
    
    const char *name = NULL;
};

class NodeBlockItem : public Node {
public:
};

class Parser {
public:
    Parser(Lexer lexer) : lexer(lexer) {
    }
    
    void parse() {
        int i = 0;
        std::vector<std::shared_ptr<NodeExternalDeclaration>> declarations;
        read_list(i, &Parser::read_external_declaration, declarations);
        
        for (auto &child : declarations)
            child->dump("");
        
        if (!eof(i))
            error("declaration expected", i);
    }
    
protected:
    Lexer lexer;
    std::vector<Token> token_queue;
    
    bool eof(int i) {
        return (lexer.has_ended() && (int)token_queue.size() < i) || peek(i).type == TokenType::END;
    }
    
    Token &peek(int i) {
        if (i < (int)token_queue.size())
            return token_queue[i];
        
        while (!lexer.has_ended() && (int)token_queue.size() <= i) {
            // printf("reading token...\n");
            token_queue.push_back(lexer.next_token());
        }
        
        return token_queue[token_queue.size() - 1]; // size() <= i+1
    }
    
    [[noreturn]] void error(const std::string &message, int offset) {
        TextPosition start_pos = peek(offset).pos;
        throw ParserError(message, start_pos);
    }
    
#pragma mark - Helpers
    
    bool read_punctuator(int &i, TokenPunctuator punctuator) {
        if (peek(i).punctuator == punctuator) {
            ++i;
            return true;
        }
        
        return false;
    }
    
    bool read_keyword(int &i, TokenKeyword keyword) {
        if (peek(i).keyword == keyword) {
            ++i;
            return true;
        }
        
        return false;
    }
    
    template<typename T>
    bool read_list(int &i, bool (Parser::*method)(int &, T &node), std::vector<T> &result) {
        int initial_i = i;
        
        while (!eof(i)) {
            T temp;
            if (!(this->*method)(i, temp))
                break;
            
            result.push_back(temp);
        }
        
        return i != initial_i;
    }
    
    template<typename T>
    bool read_separated_list(int &i, bool (Parser::*method)(int &, T &node), TokenPunctuator separator, std::vector<T> &result) {
        int initial_i = i;
        int last_good_i = i;
        
        while (!eof(i)) {
            T temp;
            if (!(this->*method)(i, temp))
                break;
            
            result.push_back(temp);
            last_good_i = i;
            
            if(!read_punctuator(i, separator))
                break;
        }
        
        i = last_good_i;
        return i != initial_i;
    }
    
#pragma mark - Declarations
    
    bool read_declarator(int &i, NodeDeclarator &node) {
        read_pointer(i, node);
        NON_EMPTY(read_direct_declarator(i, node), "direct declarator expected");
        return true;
    }
    
    bool read_declaration_specifiers(int &i, std::vector<std::shared_ptr<NodeTypeSpecifier>> &node) {
        NON_EMPTY_RET(read_list(i, &Parser::read_type_specifier, node));
        return true;
    }
    
    bool read_direct_declarator_prefix(int &i, NodeDeclarator &node) {
        // case 1
        if (read_identifier(i, node.name))
            return true;
        
        // case 2: function pointer
        if (read_punctuator(i, TokenPunctuator::RB_OPEN)) {
            NodeDeclarator declarator;
            read_declarator(i, declarator);
            node.name = declarator.name;
            std::cout << "@todo" << std::endl;
            NON_EMPTY(read_punctuator(i, TokenPunctuator::RB_CLOSE), "closing bracket expected");
            return true;
        }
        
        return false;
    }
    
    bool read_direct_declarator(int &i, NodeDeclarator &node) {
        NON_EMPTY(read_direct_declarator_prefix(i, node), "direct declarator expected");
        
        int last_good_i = i;
        
        while (!eof(i)) {
            // try reading parameter-list / identifier-list
            std::vector<NodeParameterDeclaration> parameter_list;
            std::vector<const char *> identifier_list;
            
            if (!read_punctuator(i, TokenPunctuator::RB_OPEN))
                break;
            
            if (read_parameter_list(i, parameter_list)) {
                std::cout << "@todo" << std::endl;
            } else if (read_identifier_list(i, identifier_list)) {
                std::cout << "@todo" << std::endl;
            }
            
            if (!read_punctuator(i, TokenPunctuator::RB_CLOSE))
                break;
            
            last_good_i = i;
        }
        
        i = last_good_i;
        return true;
    }
    
    bool read_identifier(int &i, const char *&text) {
        if (peek(i).type == TokenType::IDENTIFIER) {
            text = peek(i).text;
            ++i;
            
            return true;
        }
        
        return false;
    }
    
    bool read_identifier_list(int &i, std::vector<const char *> &node) {
        return read_separated_list(i, &Parser::read_identifier, TokenPunctuator::COMMA, node);
    }
    
    bool read_parameter_list(int &i, std::vector<NodeParameterDeclaration> &node) {
        NON_EMPTY(read_separated_list(i, &Parser::read_parameter_declaration, TokenPunctuator::COMMA, node),
                  "parameter list expected");
        return true;
    }
    
    bool read_parameter_declaration(int &i, NodeParameterDeclaration &node) {
        NON_EMPTY(read_declaration_specifiers(i, node.specifiers), "declaration specifiers expected");
        NON_EMPTY(read_declarator(i, node.declarator), "declarator expected");
        return true;
    }
    
    bool read_pointer_single(int &i, NodePointer &node) {
        NON_EMPTY_RET(read_punctuator(i, TokenPunctuator::ASTERISK));
        read_type_qualifier_list(i, node.specifiers);
        
        return true;
    }
    
    bool read_pointer(int &i, NodeDeclarator &node) {
        return read_list(i, &Parser::read_pointer_single, node.pointers);
    }
    
    bool read_type_qualifier_list(int &i, std::vector<std::shared_ptr<NodeTypeSpecifier>> &node) {
        return read_list(i, &Parser::read_type_specifier, node);
    }
    
    bool read_specifier_qualifier_list(int &i, std::vector<std::shared_ptr<NodeTypeSpecifier>> &node) {
        return read_list(i, &Parser::read_type_specifier, node);
    }
    
#pragma mark - Structs
    
    bool read_struct_declarator(int &i, NodeDeclarator &node) {
        return read_declarator(i, node);
    }
    
    bool read_struct_declarator_list(int &i, std::vector<NodeDeclarator> &node) {
        return read_separated_list(i, &Parser::read_struct_declarator, TokenPunctuator::COMMA, node);
    }
    
    bool read_struct_declaration(int &i, NodeDeclaration &node) {
        NON_EMPTY_RET(read_specifier_qualifier_list(i, node.specifiers));
        read_struct_declarator_list(i, node.declarators);
        NON_EMPTY(read_punctuator(i, TokenPunctuator::SEMICOLON), "semicolon expected after struct/union member");
        return true;
    }
    
    bool read_struct_declaration_list(int &i, std::vector<NodeDeclaration> &node) {
        return read_list(i, &Parser::read_struct_declaration, node);
    }
    
    bool read_struct_body(int &i, NodeTypeComposed &node) {
        NON_EMPTY_RET(read_punctuator(i, TokenPunctuator::CB_OPEN));
        
        NON_EMPTY(read_struct_declaration_list(i, node.declarations), "declaration list expected");
        NON_EMPTY(read_punctuator(i, TokenPunctuator::CB_CLOSE), "missing closing bracket");
        
        return true;
    }
    
    bool read_type_specifier(int &i, std::shared_ptr<NodeTypeSpecifier> &node) {
        Token &token = peek(i);
        switch (token.keyword) {
            case TokenKeyword::VOID:
            case TokenKeyword::CHAR:
            case TokenKeyword::SHORT:
            case TokenKeyword::INT:
            case TokenKeyword::LONG:
            case TokenKeyword::FLOAT:
            case TokenKeyword::DOUBLE:
            case TokenKeyword::SIGNED:
            case TokenKeyword::UNSIGNED:
            case TokenKeyword::_BOOL:
            case TokenKeyword::_COMPLEX:
                ++i;
                node = std::make_shared<NodeTypeNamed>(token.text);
                
                return true;
                
            case TokenKeyword::STRUCT:
            case TokenKeyword::UNION: {
                ++i;
                
                NodeTypeComposed *n = new NodeTypeComposed();
                node.reset(n);
                
                n->type = token.keyword;
                
                bool has_identifier = read_identifier(i, n->name);
                bool has_body = peek(i).punctuator == TokenPunctuator::CB_OPEN;
                
                if (!has_body && !has_identifier)
                    error("struct/union without identifier or body", i);
                
                if (has_body)
                    NON_EMPTY(read_struct_body(i, *n), "struct/union body expected");
                
                return true;
            }
                
            default:
                return false;
        }
        
        return false;
    }
    
    bool read_initializer(int &i, NodeDeclarator &node) {
        error("not yet implemented", i);
    }
    
    bool read_init_declarator(int &i, NodeDeclarator &node) {
        NON_EMPTY_RET(read_declarator(i, node));
        
        if (read_punctuator(i, TokenPunctuator::ASSIGN)) {
            // also read initializer
            NON_EMPTY(read_initializer(i, node), "initializer expected");
        }
        
        return true;
    }
    
    bool read_init_declarator_list(int &i, std::vector<NodeDeclarator> &node) {
        return read_separated_list(i, &Parser::read_init_declarator, TokenPunctuator::COMMA, node);
    }
    
    bool read_declaration(int &i, NodeDeclaration &node) {
        NON_EMPTY_RET(read_declaration_specifiers(i, node.specifiers));
        read_init_declarator_list(i, node.declarators);
        NON_EMPTY(read_punctuator(i, TokenPunctuator::SEMICOLON), "semicolon expected");
        
        return true;
    }
    
    bool read_declaration_list(int &i, std::vector<NodeDeclaration> &node) {
        return read_list(i, &Parser::read_declaration, node);
    }
    
    bool read_external_declaration(int &i, std::shared_ptr<NodeExternalDeclaration> &node) {
        std::vector<std::shared_ptr<NodeTypeSpecifier>> specifiers;
        NodeDeclarator declarator;
        
        NON_EMPTY_RET(read_declaration_specifiers(i, specifiers));
        NON_EMPTY(read_declarator(i, declarator), "declarator expected");
        
        bool needs_declaration_list = peek(i).punctuator == TokenPunctuator::COMMA;
        bool needs_initialization = peek(i).punctuator == TokenPunctuator::ASSIGN;
        bool is_declaration = needs_initialization || needs_declaration_list || peek(i).punctuator == TokenPunctuator::SEMICOLON;
        
        if (is_declaration) {
            // declaration: ... (',' init-declarator-list(opt))(opt) ;
            
            auto n = new NodeExternalDeclarationVariable();
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            
            if (needs_initialization) {
                ++i; // consume assign
                read_initializer(i, declarator);
                
                needs_initialization = peek(i).punctuator == TokenPunctuator::COMMA;
            }
            
            node->declarators.push_back(std::move(declarator));
            
            if (needs_declaration_list) {
                ++i; // jump over comma
                read_init_declarator_list(i, node->declarators);
            }
            
            NON_EMPTY(read_punctuator(i, TokenPunctuator::SEMICOLON), "semicolon expected");
            
            return true;
        } else {
            // function definition: ... declaration-list(opt) compound-statement
            
            auto n = new NodeExternalDeclarationFunction;
            node.reset(n);
            
            node->specifiers = std::move(specifiers);
            node->declarators.push_back(std::move(declarator));
            
            read_declaration_list(i, n->declarations);
            NON_EMPTY(read_compound_statement(i), "compound statement expected");
            
            return true;
        }
        
        // init-declarator-list: read_list(init-declarator, ',')
        // init-declarator: declarator
        //                | declarator '=' initializer
        
        error("not yet implemented", i);
    }
    
#pragma mark - Statements
    
    bool read_statement(int &i) {
        if (read_labeled_statement(i)) {
        } else if (read_compound_statement(i)) {
        } else if (read_expression_statement(i)) {
        } else if (read_selection_statement(i)) {
        } else if (read_iteration_statement(i)) {
        } else if (read_jump_statement(i)) {
        } else
            return false;
        
        return true;
    }
    
    bool read_labeled_statement(int &i) {
        if (read_keyword(i, TokenKeyword::CASE)) {
        } else if (read_keyword(i, TokenKeyword::DEFAULT)) {
        } else if (peek(i).type == TokenType::IDENTIFIER && peek(i + 1).punctuator == TokenPunctuator::COLON) {
        } else
            return false;
        
        read_statement(i);
        return true;
    }
    
    bool read_block_item(int &i, NodeBlockItem &node) {
        NodeDeclaration declaration;
        
        if (read_declaration(i, declaration)) {
        } else if (read_statement(i)) {
        } else
            return false;
        
        return true;
    }
    
    bool read_block_item_list(int &i, std::vector<NodeBlockItem> &node) {
        return read_list(i, &Parser::read_block_item, node);
    }
    
    bool read_compound_statement(int &i) {
        std::vector<NodeBlockItem> block_items;
        
        NON_EMPTY_RET(read_punctuator(i, TokenPunctuator::CB_OPEN));
        read_block_item_list(i, block_items);
        NON_EMPTY(read_punctuator(i, TokenPunctuator::CB_CLOSE), "closing bracket after compound statement expected");
        
        return true;
    }
    
    bool read_expression_statement(int &i) {
        int j = i;
        
        read_expression(j);
        NON_EMPTY_RET(read_punctuator(j, TokenPunctuator::SEMICOLON));
        
        i = j;
        
        return true;
    }
    
    bool read_selection_statement(int &i) {
        NON_EMPTY_RET(read_keyword(i, TokenKeyword::IF));
        NON_EMPTY(read_punctuator(i, TokenPunctuator::RB_OPEN), "opening bracket expected");
        NON_EMPTY(read_expression(i), "expression expected");
        NON_EMPTY(read_punctuator(i, TokenPunctuator::RB_CLOSE), "closing bracket expected");
        NON_EMPTY(read_statement(i), "statement expected");
        
        if (read_keyword(i, TokenKeyword::ELSE))
            NON_EMPTY(read_statement(i), "statement expected");
        
        return true;
    }
    
    bool read_iteration_statement(int &i) {
        NON_EMPTY_RET(read_keyword(i, TokenKeyword::WHILE));
        NON_EMPTY(read_punctuator(i, TokenPunctuator::RB_OPEN), "opening bracket expected");
        NON_EMPTY(read_expression(i), "expression expected");
        NON_EMPTY(read_punctuator(i, TokenPunctuator::RB_CLOSE), "closing bracket expected");
        NON_EMPTY(read_statement(i), "statement expected");
        
        return true;
    }
    
    bool read_jump_statement(int &i) {
        if (read_keyword(i, TokenKeyword::GOTO)) {
            const char *target;
            NON_EMPTY(read_identifier(i, target), "identifier expected");
        } else if (read_keyword(i, TokenKeyword::CONTINUE)) {
        } else if (read_keyword(i, TokenKeyword::BREAK)) {
        } else if (read_keyword(i, TokenKeyword::RETURN)) {
            read_expression(i);
        } else
            return false;
        
        NON_EMPTY(read_punctuator(i, TokenPunctuator::SEMICOLON), "semicolon expected");
        
        return true;
    }
    
#pragma mark - Expressions
    
    bool read_expression(int &i) {
        // @todo
        return false;
    }
};

#undef NON_EMPTY
#undef NON_EMPTY_RET

#endif /* Parser_hpp */
