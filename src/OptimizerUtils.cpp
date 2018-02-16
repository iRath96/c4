#include "OptimizerUtils.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wconversion"
#include <llvm/Support/raw_ostream.h>
#pragma GCC diagnostic pop


using namespace std;
using namespace llvm;

void debug_print(string prefix, Value *value, bool printValue) {
	if (!debug_mode) return;

	cout << prefix << " (" << value << ")";
	if (printValue) value->print(outs());
	cout << endl;
}

ostream &operator<<(ostream &os, OptimizerPass::ValueDomain const &vd) {
	if (vd.isDead) os << "dead ";
    if (vd.isBottom) return os << "bottom";
    if (vd.isTop()) return os << "top";
    return os << "[" << vd.min << ";" << vd.max << "]";
}

ostream &operator<<(ostream &os, OptimizerPass::ConstraintSet const &cs) {
	if (cs.isBottom) return os << "  constraints: bottom" << endl;
	for (auto &pred : cs.predicates) {
		pred.first.first->print(errs());
		switch (pred.second) {
		case CmpInst::ICMP_EQ:  os << " == "; break;
		case CmpInst::ICMP_NE:  os << " != "; break;
		case CmpInst::ICMP_SLE: os << " <= "; break;
		case CmpInst::ICMP_SLT: os << " < ";  break;
		case CmpInst::ICMP_SGE: os << " >= "; break;
		case CmpInst::ICMP_SGT: os << " > ";  break;
		default: os << " ? ";
		}
		pred.first.second->print(errs());
		os << endl;
	}

	return os;
}

ostream &operator<<(ostream &os, OptimizerPass::BlockDomain const &bd) {
	if (bd.isEntry) os << " entry";
	if (bd.reachable) os << " reachable";
	else os << " unreachable";
	os << endl << bd.cs;

	for (auto &edge : bd.edges) {
		os << "  from " << edge.first->getName().str();
		if (edge.second.cond)
			os << " (" << edge.second.cond->getName().str() << " = " << (edge.second.condV ? "true)" : "false)");
		os << endl;
	}

	for (auto &dom : bd.dominators)
		os << "  dom " << dom->getName().str() << endl;

	return os;
}
