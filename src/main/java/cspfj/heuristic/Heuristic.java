package cspfj.heuristic;

import cspfj.problem.Problem;

public interface Heuristic {
	Pair selectPair(Problem problem);
	VariableHeuristic getVariableHeuristic();
	ValueHeuristic getValueHeuristic();
	void compute();
}
