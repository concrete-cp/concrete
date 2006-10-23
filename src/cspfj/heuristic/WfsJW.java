package cspfj.heuristic;

import java.util.HashMap;
import java.util.Map;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.constraint.Constraint;
import cspfj.constraint.MatrixManager;

public final class WfsJW implements Heuristic {

	final private Variable[] variables;

	final private Map<Constraint, float[][]> scores;

	public WfsJW(Problem problem) {
		variables = problem.getVariables();
		scores = new HashMap<Constraint, float[][]>();

		for (Constraint c : problem.getConstraints()) {
			final float[][] score = new float[c.getArity()][];
			scores.put(c, score);
			for (int p = c.getArity(); --p >= 0;) {
				final Variable variable = c.getInvolvedVariables()[p];
				score[p] = new float[variable.getDomain().length];
				for (int i = variable.getFirst(); i >= 0; i = variable
						.getNext(i)) {
					score[p][i] = getStaticScore(c, p, i);
				}

			}
		}

	}

	public void compute() {
		// nope
	}

	private float getStaticScore(final Constraint constraint,
			final int position, final int index) {
		double tot = Math
				.pow(2, -1 - constraint.getNbSupports(position, index));

		final int[] tuple = constraint.getTuple();
		final int arity = constraint.getArity();
		final MatrixManager matrix = constraint.getMatrix();

		matrix.setFirstTuple(position, index);
		do {
			if (!constraint.check()) {
				continue;
			}

			for (int j = arity; --j >= 0;) {
				if (j != position) {
					tot += Math.pow(2, -1
							- constraint.getNbSupports(j, tuple[j]));
				}
			}
		} while (matrix.next());

		return (float) tot;
	}

	public Pair selectPair() {
		Variable bestVariable = null;
		int bestValue = -1;
		float bestScore = 0;

		for (Variable v : variables) {
			if (v.getDomainSize() <= 1) {
				continue;
			}
			for (int i = v.getFirst(); i >= 0; v.getNext(i)) {
				float score = (float) Math.pow(2, -v.getDomainSize());

				for (Constraint c : v.getInvolvingConstraints()) {
					score += c.getWeight() * scores.get(c)[c.getPosition(v)][i];
				}

				if (bestValue < 0 || score > bestScore) {
					bestVariable = v;
					bestValue = i;
					bestScore = score;
				}
			}
		}

		if (bestVariable == null) {
			for (Variable v : variables) {
				if (!v.isAssigned()) {
					return new Pair(v, v.getFirst());
				}
			}
		}

		return new Pair(bestVariable, bestValue);
	}
}
