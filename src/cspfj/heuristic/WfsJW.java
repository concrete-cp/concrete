package cspfj.heuristic;

import java.util.HashMap;
import java.util.Map;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.constraint.Constraint;
import cspfj.constraint.MatrixManager;

public final class WfsJW implements Heuristic {

	final private Variable[] variables;

	final private Map<Constraint, double[][]> scores;

	public WfsJW(Problem problem) {
		variables = problem.getVariables();
		scores = new HashMap<Constraint, double[][]>();

		for (Constraint c : problem.getConstraints()) {
			final double[][] score = new double[c.getArity()][];
			scores.put(c, score);
			for (int p = c.getArity(); --p >= 0;) {
				final Variable variable = c.getInvolvedVariables()[p];
				score[p] = new double[variable.getDomain().length];
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

	private double getStaticScore(final Constraint constraint,
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

		return (double) tot;
	}

	public int selectPair() {
		Variable bestVariable = null;
		int bestValue = -1;
		double bestScore = 0;

		for (Variable v : variables) {
			if (v.getDomainSize() <= 1) {
				continue;
			}
			for (int i = v.getFirst(); i >= 0; v.getNext(i)) {
				double score = (double) Math.pow(2, -v.getDomainSize());

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
					return Pair.pair(v, v.getFirst());
				}
			}
		}

		return Pair.pair(bestVariable, bestValue);
	}
}
