//
//  Parser.cpp
//  c4
//
//  Created by Alexander Rath on 04.11.17.
//  Copyright © 2017 Alexander Rath. All rights reserved.
//

#include "Parser.h"

DebugTree dbg_tree_root;
DebugTree *dbg_tree_current = &dbg_tree_root;

void DebugTree::dump(Parser *parser, std::string indent) {
    //if (has_returned && !ret_val) return;
    
    std::cout << indent << function;
    if (has_returned)
        std::cout << " = " << (ret_val ? "ACCEPT" : "DENY");
    else
        std::cout << " -- error";
    std::cout << std::endl;
    
    //if (!has_returned || ret_val) {
        std::cout << indent;
        parser->print_context(start_index);
        if (has_returned) {
            std::cout << indent;
            parser->print_context(end_index);
        }
        std::cout << std::endl;
    //}
    
    for (auto &child : children)
        child->dump(parser, indent + "  ");
}

const char *operator_name(TokenPunctuator punctuator) {
    switch (punctuator) {
        case TokenPunctuator::NOT_A_PUNCTUATOR: return "+";
            
        case TokenPunctuator::CB_OPEN: return "{";
        case TokenPunctuator::CB_CLOSE: return "}";
        case TokenPunctuator::SB_OPEN: return "[";
        case TokenPunctuator::SB_CLOSE: return "]";
        case TokenPunctuator::RB_OPEN: return "(";
        case TokenPunctuator::RB_CLOSE: return ")";
            
        case TokenPunctuator::QMARK: return "?";
        case TokenPunctuator::COLON: return ":";
            
        case TokenPunctuator::PLUS_ASSIGN: return "+=";
        case TokenPunctuator::MINUS_ASSIGN: return "-=";
        case TokenPunctuator::MUL_ASSIGN: return "*=";
        case TokenPunctuator::DIV_ASSIGN: return "/=";
        case TokenPunctuator::MODULO_ASSIGN: return "%=";
        case TokenPunctuator::BIT_OR_ASSIGN: return "|=";
        case TokenPunctuator::BIT_AND_ASSIGN: return "&=";
        case TokenPunctuator::BIT_XOR_ASSIGN: return "^=";
        case TokenPunctuator::RSHIFT_ASSIGN: return ">>=";
        case TokenPunctuator::LSHIFT_ASSIGN: return "<<=";
        case TokenPunctuator::ASSIGN: return "=";
            
        case TokenPunctuator::LOG_OR: return "||";
        case TokenPunctuator::LOG_AND: return  "&&";
        case TokenPunctuator::BIT_OR: return "|";
        case TokenPunctuator::BIT_XOR: return "^";
        case TokenPunctuator::BIT_AND: return "&";
            
        case TokenPunctuator::CMP_EQ: return "==";
        case TokenPunctuator::CMP_NEQ: return "!=";
            
        case TokenPunctuator::AB_OPEN: return "<";
        case TokenPunctuator::AB_CLOSE: return ">";
        case TokenPunctuator::CMP_LTE: return "<=";
        case TokenPunctuator::CMP_GTE: return ">=";
            
        case TokenPunctuator::RSHIFT: return ">>";
        case TokenPunctuator::LSHIFT: return "<<";
            
        case TokenPunctuator::PLUS: return "+";
        case TokenPunctuator::MINUS: return "-";
            
        case TokenPunctuator::ASTERISK: return "*";
        case TokenPunctuator::SLASH: return "/";
        case TokenPunctuator::MODULO: return "%";
            
        case TokenPunctuator::PLUSPLUS: return "++";
        case TokenPunctuator::MINUSMINUS: return "--";
        case TokenPunctuator::BIT_NOT: return "~";
        case TokenPunctuator::LOG_NOT: return "!";
            
        case TokenPunctuator::DOUBLE_HASH: return "##";
        case TokenPunctuator::HASH: return "#";
        case TokenPunctuator::ELIPSES: return "...";
        case TokenPunctuator::PERIOD: return ".";
        case TokenPunctuator::COMMA: return ",";
        case TokenPunctuator::SEMICOLON: return ";";
        case TokenPunctuator::ARROW: return "->";
    }
    
    return "(error)";
}
