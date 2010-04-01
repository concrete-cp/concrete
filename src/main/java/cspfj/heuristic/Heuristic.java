package cspfj.heuristic;

import cspfj.problem.Problem;

public interface Heuristic {
	Pair selectPair(Problem problem);
	void compute();
}
