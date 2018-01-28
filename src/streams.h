#ifndef streams_h
#define streams_h

#include <stddef.h>
#include <vector>

/**
 * Streams allow different units to be connected seamlessly.
 * Important concepts are _pipelines_, _nodes_ and _fragments_.
 * Pipelines are networks of nodes, which solve a given task
 * (e. g. InputFile -> Lexer -> Parser -> Compiler -> OutputFile).
 * Nodes describe the units within the network. These can generate fragments
 * and/or accept fragments. And lastly, a fragment is essentially a packet of
 * data.
 */
namespace streams {

/**
 * Represents a node in a pipeline which generates fragments on request.
 * It has no inputs and exactly one `Stream` should be connected to it.
 * @tparam P The output fraagment data type.
 */
template<typename P>
class Source {
public:
	typedef P Output;

	/**
	 * Queries this node for the next fragment.
	 * @param[out] fragment
	 *   A pointer to where the resulting fragment should be written.
	 *   This will be left untouched in case of failure.
	 * @return `true` if the operation was succesful, `false` if no new fragment is available.
	 */
	virtual bool next(Output *fragment) = 0;

	virtual ~Source() {}
};

/**
 * Represents a node in a pipeline which accepts fragments from a source and generates
 * new fragments in a stateful manner. When asked to generate a fragment this
 * node should ask the source it is connected to for one or more fragment which are then
 * operated on. During process it is allowed to generate one or more fragments based on
 * the input fragments.
 * @tparam P The input fragment data type.
 * @tparam Q The output fragment data type.
 */
template<typename P, typename Q>
class Stream : public Source<Q> {
protected:
	Source<P> *source;
public:
	typedef P Input;

	Stream(Source<P> *source) : source(source) {}

	/**
	 * Requests fragments from the source and transforms these until no more fragments
	 * are available from the source.
	 */
	void drain() {
		P output;
		while (this->next(&output));
	}
};

/**
 * Represents a node in a pipeline which performs operations on fragments but does
 * not generate new fragments.
 * @tparam P The input fragment data type.
 */
template<typename P>
class Sink : public Stream<P, void> {
public:
	Sink(Source<P> *source) : Stream<P, void>(source) {}
};

template<typename P>
class Buffer;

/**
 * Used by `Buffer` to allow multiple streams to be connected to the
 * same source. Use a `Buffer` to create instances of this.
 * @see Buffer
 */
template<typename P>
class BufferChild : public Source<P> {
protected:
	friend class Buffer<P>;

	Buffer<P> *parent;
	size_t index = 0;
	BufferChild(Buffer<P> *parent) : parent(parent) {}

public:
	virtual bool next(P *);
};

/**
 * Allows multiple streams to be connected to the same source.
 * For this to work, a buffer can spwan several `BufferChild`ren
 * which can then be used as sources for other streams.
 * @tparam P The input fragment data type.
 * @see BufferChild
 */
template<typename P>
class Buffer : public Sink<P> {
protected:
	friend class BufferChild<P>;

	std::vector<P> items;
	std::vector<BufferChild<P> *> children;

	bool get(size_t index, P *output) {
		if (index < items.size()) {
			*output = items[index];
			return true;
		}
		
		while (index >= items.size() && this->source->next(output)) items.push_back(*output);
		return index < items.size();
	}

	bool hasEnded(size_t index) const {
		return index >= items.size() && this->source->hasEnded();
	}

public:
	typedef BufferChild<P> Child;

	Buffer(Source<P> *source) : Sink<P>(source) {}
	~Buffer() {
		for (const auto &child : children) delete child;
	}

	/**
	 * Returns a new BufferChild that can be used as Source.
	 * The first fragment that this child returns will be the first fragment
	 * that was fetched from the Buffer's source. If the BufferChild is
	 * asked for the next fragment, it will first look into the buffer of its
	 * parent. If no next fragment is available in the buffer, the source
	 * will be queried for a new fragment. If the source returns false,
	 * the BufferChild will also return false.
	 * @return A new BufferChild connected to this Buffer.
	 */
	Child *createChild();

	/**
	 * This method exists so that drain can be used on a Buffer.
	 * This will then drain the source of this Buffer.
	 */
	virtual bool next(void *) {
		P item;
		if (!this->source->next(&item)) return false;
		items.push_back(item);
		return true;
	}
};

template<typename P>
BufferChild<P> *Buffer<P>::createChild() {
	auto bc = new BufferChild<P>(this);
	children.push_back(bc);
	return bc;
}

template<typename P>
bool BufferChild<P>::next(P *output) {
	return parent->get(index++, output);
}

}

#endif /* streams_h */
