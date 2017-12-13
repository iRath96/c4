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
	using P = Token::Punctuator;

	switch (punctuator) {
		case P::NOT_A_PUNCTUATOR: return "(nap)";
		case P::NEVER: return "(never)";

		case P::CB_OPEN: return "{";
		case P::CB_CLOSE: return "}";
		case P::SB_OPEN: return "[";
		case P::SB_CLOSE: return "]";
		case P::RB_OPEN: return "(";
		case P::RB_CLOSE: return ")";

		case P::QMARK: return "?";
		case P::COLON: return ":";

		case P::PLUS_ASSIGN: return "+=";
		case P::MINUS_ASSIGN: return "-=";
		case P::MUL_ASSIGN: return "*=";
		case P::DIV_ASSIGN: return "/=";
		case P::MODULO_ASSIGN: return "%=";
		case P::BIT_OR_ASSIGN: return "|=";
		case P::BIT_AND_ASSIGN: return "&=";
		case P::BIT_XOR_ASSIGN: return "^=";
		case P::RSHIFT_ASSIGN: return ">>=";
		case P::LSHIFT_ASSIGN: return "<<=";
		case P::ASSIGN: return "=";

		case P::LOG_OR: return "||";
		case P::LOG_AND: return  "&&";
		case P::BIT_OR: return "|";
		case P::BIT_XOR: return "^";
		case P::BIT_AND: return "&";

		case P::CMP_EQ: return "==";
		case P::CMP_NEQ: return "!=";

		case P::AB_OPEN: return "<";
		case P::AB_CLOSE: return ">";
		case P::CMP_LTE: return "<=";
		case P::CMP_GTE: return ">=";

		case P::RSHIFT: return ">>";
		case P::LSHIFT: return "<<";

		case P::PLUS: return "+";
		case P::MINUS: return "-";

		case P::ASTERISK: return "*";
		case P::SLASH: return "/";
		case P::MODULO: return "%";

		case P::PLUSPLUS: return "++";
		case P::MINUSMINUS: return "--";
		case P::BIT_NOT: return "~";
		case P::LOG_NOT: return "!";

		case P::DOUBLE_HASH: return "##";
		case P::HASH: return "#";
		case P::ELIPSES: return "...";
		case P::PERIOD: return ".";
		case P::COMMA: return ",";
		case P::SEMICOLON: return ";";
		case P::ARROW: return "->";
	}

	return "(error)";
}
