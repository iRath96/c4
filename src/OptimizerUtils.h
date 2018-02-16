#ifndef OptimizerUtils_h
#define OptimizerUtils_h

#include <set>
#include <iostream>

#include "OptimizerPass.h"


extern bool debug_mode;


void debug_print(std::string prefix, llvm::Value *value, bool printValue = true);
std::ostream &operator<<(std::ostream &os, OptimizerPass::ValueDomain const &vd);
std::ostream &operator<<(std::ostream &os, OptimizerPass::ConstraintSet const &cs);
std::ostream &operator<<(std::ostream &os, OptimizerPass::BlockDomain const &bd);

// modified from:
// https://stackoverflow.com/questions/24263259/c-stdseterase-with-stdremove-if
template <class T, class Comp, class Alloc, class Predicate>
bool discard_if(std::set<T, Comp, Alloc> &c, Predicate pred) {
	bool has_changed = false;
    for (auto it{c.begin()}, end{c.end()}; it != end; ) {
        if (pred(*it)) {
        	it = c.erase(it);
        	has_changed = true;
        } else ++it;
    }

    return has_changed;
}

#endif /* OptimizerUtils_h */
