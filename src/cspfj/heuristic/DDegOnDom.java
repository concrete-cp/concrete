package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class DDegOnDom extends AbstractVariableHeuristic {

    public DDegOnDom(Problem problem) {
		super(problem);
	}

	public int compare(final Variable variable0, final Variable variable1) {
        final float result = variable1.getDDeg() / variable1.getDomainSize()
                - variable0.getDDeg() / variable0.getDomainSize();

        if (result == 0) {
            return variable0.getId() - variable1.getId();
        }

        return result > 0 ? 1 : -1;
        
    }

}
