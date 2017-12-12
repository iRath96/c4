#include "Parser.h"

DebugTree dbg_tree_root;
DebugTree *dbg_tree_current = &dbg_tree_root;

void DebugTree::dump(Parser *parser, std::string indent) {
    //if (has_returned && !ret_val) return;
    
    std::cout << indent << function;
    if (has_returned) std::cout << " = " << (ret_val ? "ACCEPT" : "DENY");
    else std::cout << " -- error";
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
    
    for (auto &child : children) child->dump(parser, indent + "  ");
}

const char *operator_name(Token::Punctuator punctuator) {
    switch (punctuator) {
        case Token::Punctuator::NOT_A_PUNCTUATOR: return "(nap)";
        case Token::Punctuator::NEVER: return "(never)";
            
        case Token::Punctuator::CB_OPEN: return "{";
        case Token::Punctuator::CB_CLOSE: return "}";
        case Token::Punctuator::SB_OPEN: return "[";
        case Token::Punctuator::SB_CLOSE: return "]";
        case Token::Punctuator::RB_OPEN: return "(";
        case Token::Punctuator::RB_CLOSE: return ")";
            
        case Token::Punctuator::QMARK: return "?";
        case Token::Punctuator::COLON: return ":";
            
        case Token::Punctuator::PLUS_ASSIGN: return "+=";
        case Token::Punctuator::MINUS_ASSIGN: return "-=";
        case Token::Punctuator::MUL_ASSIGN: return "*=";
        case Token::Punctuator::DIV_ASSIGN: return "/=";
        case Token::Punctuator::MODULO_ASSIGN: return "%=";
        case Token::Punctuator::BIT_OR_ASSIGN: return "|=";
        case Token::Punctuator::BIT_AND_ASSIGN: return "&=";
        case Token::Punctuator::BIT_XOR_ASSIGN: return "^=";
        case Token::Punctuator::RSHIFT_ASSIGN: return ">>=";
        case Token::Punctuator::LSHIFT_ASSIGN: return "<<=";
        case Token::Punctuator::ASSIGN: return "=";
            
        case Token::Punctuator::LOG_OR: return "||";
        case Token::Punctuator::LOG_AND: return  "&&";
        case Token::Punctuator::BIT_OR: return "|";
        case Token::Punctuator::BIT_XOR: return "^";
        case Token::Punctuator::BIT_AND: return "&";
            
        case Token::Punctuator::CMP_EQ: return "==";
        case Token::Punctuator::CMP_NEQ: return "!=";
            
        case Token::Punctuator::AB_OPEN: return "<";
        case Token::Punctuator::AB_CLOSE: return ">";
        case Token::Punctuator::CMP_LTE: return "<=";
        case Token::Punctuator::CMP_GTE: return ">=";
            
        case Token::Punctuator::RSHIFT: return ">>";
        case Token::Punctuator::LSHIFT: return "<<";
            
        case Token::Punctuator::PLUS: return "+";
        case Token::Punctuator::MINUS: return "-";
            
        case Token::Punctuator::ASTERISK: return "*";
        case Token::Punctuator::SLASH: return "/";
        case Token::Punctuator::MODULO: return "%";
            
        case Token::Punctuator::PLUSPLUS: return "++";
        case Token::Punctuator::MINUSMINUS: return "--";
        case Token::Punctuator::BIT_NOT: return "~";
        case Token::Punctuator::LOG_NOT: return "!";
            
        case Token::Punctuator::DOUBLE_HASH: return "##";
        case Token::Punctuator::HASH: return "#";
        case Token::Punctuator::ELIPSES: return "...";
        case Token::Punctuator::PERIOD: return ".";
        case Token::Punctuator::COMMA: return ",";
        case Token::Punctuator::SEMICOLON: return ";";
        case Token::Punctuator::ARROW: return "->";
    }
    
    return "(error)";
}
