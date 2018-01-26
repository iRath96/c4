#ifndef common_h
#define common_h

namespace common {

struct TextPosition {
	unsigned int index  = 0;
	unsigned int line   = 1;
	unsigned int column = 1;
};

class Error {
public:
	std::string message;
	TextPosition pos;

	Error(const std::string &message, TextPosition pos)
	: message(message), pos(pos) {}

	virtual ~Error() {}
	virtual void raise() { throw *this; }
};

}

#endif /* common_h */
