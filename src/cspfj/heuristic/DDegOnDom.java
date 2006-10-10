package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class DDegOnDom extends AbstractVariableHeuristic {

    public DDegOnDom(Problem problem) {
		super(problem);
	}

	public float getScore(final Variable variable) {
        return variable.getDDeg() / variable.getDomainSize();
    }

}
