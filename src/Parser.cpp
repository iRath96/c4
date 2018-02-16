#include "Parser.h"


namespace parser {

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

}
