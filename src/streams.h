#ifndef streams_h
#define streams_h

#include <stdint.h>
#include <vector>

template<typename P>
class Source {
public:
	typedef P Output;
	virtual bool next(Output *) = 0;
	virtual ~Source() {}
};

template<typename P, typename Q>
class Stream : public Source<Q> {
protected:
	Source<P> *source;
public:
	typedef P Input;
	Stream(Source<P> *source) : source(source) {}
	void drain() {
		P output;
		while (this->next(&output));
	}
};

template<typename P>
class Sink : public Stream<P, void> {
public:
	Sink(Source<P> *source) : Stream<P, void>(source) {}
};

template<typename P>
class Buffer;

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

	Child *createChild();

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

#endif /* streams_h */
