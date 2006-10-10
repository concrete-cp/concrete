package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class Lexico extends AbstractValueHeuristic {

    public Lexico(Problem problem) {
        super(problem);
    }

    public int selectIndex(final Variable variable) {
        return variable.getFirstPresentIndex();
    }

    public float getScore(final Variable var, final int index) {
        return index;
    }

}
