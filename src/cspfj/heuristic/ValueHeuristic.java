package cspfj.heuristic;

import cspfj.problem.Variable;

public interface ValueHeuristic {
	void init(Variable variable);
	int selectIndex(Variable variable) ;
	
}
