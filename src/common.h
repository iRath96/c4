#ifndef common_h
#define common_h

/**
 * Provides common classes that are used across all components.
 */
namespace common {

/**
 * Indicates a location within the input.
 * @todo Maybe also put a reference to the source text.
 */
struct TextPosition {
	unsigned int index  = 0;
	unsigned int line   = 1;
	unsigned int column = 1;
};

/**
 * The base class for all errors thrown within our streams.
 */
class Error {
public:
	/** A human readable description of the error. */
	std::string message;
	/** The location in the input where the error occured. */
	TextPosition pos;

	Error(const std::string &message, TextPosition pos)
	: message(message), pos(pos) {}

	virtual ~Error() {}
	virtual void raise() { throw *this; }
};

}

#endif /* common_h */
