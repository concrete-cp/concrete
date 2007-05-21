package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Rand extends AbstractStaticValueHeuristic {

	private final static Random random = new Random();
	
	public Rand(Problem problem, boolean failFirst) {
		super(problem, failFirst);
	}

	public double getScore(final Variable variable, final int index) {
		return random.nextDouble();
	}
	
	public void myCompute() {
		// Nothing to do...
	}
}
