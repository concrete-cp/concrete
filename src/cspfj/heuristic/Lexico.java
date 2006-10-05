package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Lexico extends AbstractValueHeuristic {

	public Lexico(Problem problem) {
		super(problem);
	}

	public void init(final Variable variable) {
		// Nothing to do 
	}

	public int selectIndex(final Variable variable) {
		return variable.getFirstPresentIndex();
	}

	
	
}
