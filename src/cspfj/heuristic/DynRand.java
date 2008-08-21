package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class DynRand extends AbstractValueHeuristic {

	final static Random random = new Random();
	
	public DynRand(final Problem problem, final boolean ff) {
		super(problem);
	}

	public void compute() {
		// TODO Auto-generated method stub

	}

	public double getScore(final Variable variable, final int index) {
		return random.nextDouble();
	}
}
