package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Rand extends AbstractStaticValueHeuristic {

	private final static Random random = new Random();
	
	public Rand(Problem problem) {
		super(problem);
	}

	public float getScore(final Variable variable, final int index) {
		return random.nextFloat();
	}

}
