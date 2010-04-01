package cspfj.heuristic;

import cspfj.problem.Variable;

public interface ValueHeuristic {
    int selectIndex(Variable variable);

    double getScore(final Variable variable, final int index);

    void compute();
}
