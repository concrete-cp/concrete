package cspfj.heuristic;

import cspfj.problem.Problem;

public interface Heuristic {
	long selectPair(Problem problem);

	void compute();
}
