package cspfj.heuristic;

import cspfj.problem.Variable;

public interface ValueHeuristic {
	int selectIndex(Variable variable) ;
    float getScore(final Variable variable, final int index) ;
    void compute() ;
}
