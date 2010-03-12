package cspfj.heuristic;

import cspfj.problem.IntVariable;

public interface ValueHeuristic {
	int selectIndex(IntVariable variable) ;
    double getScore(final IntVariable variable, final int index) ;
    void compute() ;
}
