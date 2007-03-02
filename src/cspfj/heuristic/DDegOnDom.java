package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class DDegOnDom extends AbstractVariableHeuristic {

	public double getScore(final Variable variable) {
        return variable.getDDeg() / variable.getDomainSize();
    }
	
	public String toString() {
		return "max-ddeg/dom";
	}
}
