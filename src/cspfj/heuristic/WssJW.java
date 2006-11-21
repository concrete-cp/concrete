package cspfj.heuristic;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.constraint.Constraint;
import cspfj.constraint.MatrixManager;

public final class WssJW implements Heuristic {

	final private Variable[] variables;

	final private Constraint[] constraints;

	final private Map<Variable, Integer> domainSizes;

	final private Map<Constraint, Integer> weights;

	final private Map<Variable, double[]> scores;

	final private Set<Variable> recompute;

	public WssJW(Problem problem) {
		variables = problem.getVariables();
		constraints = problem.getConstraints();
		domainSizes = new HashMap<Variable, Integer>();
		weights = new HashMap<Constraint, Integer>();
		scores = new HashMap<Variable, double[]>();
		recompute = new TreeSet<Variable>();

		for (Variable v : variables) {
			scores.put(v, new double[v.getDomain().length]);
		}

	}

	public void compute() {
		// nope
	}

	private double getScore(final Variable variable, final int index) {
		double tot = Math.pow(2, -variable.getDomainSize());
		for (Constraint c : variable.getInvolvingConstraints()) {
			tot += c.getWeight()
					* Math.pow(2, -1 - c.getNbSupports(variable, index));

			final int[] tuple = c.getTuple();
			final int position = c.getPosition(variable);
			final int arity = c.getArity();
			final MatrixManager matrix = c.getMatrix();

			matrix.setFirstTuple(position, index);
			do {
				if (!c.check()) {
					continue;
				}

				for (int j = arity; --j >= 0;) {
					if (j != position) {
						tot += c.getWeight()
								* Math.pow(2, -1
										- c.getNbSupports(c
												.getInvolvedVariables()[j],
												tuple[j]));
						// System.out
						// .print(" + 2^(-1-"
						// + supports
						// .get(c
						// .getInvolvedVariables()[j]
						// .getId())[j]
						// + ")");
					}
				}
			} while (matrix.next());
		}
		return tot;
	}

	private void recompute() {
		final Set<Variable> recompute = this.recompute;

		if (domainSizes.size() < 1) {
			recompute.addAll(Arrays.asList(variables));

			for (Constraint c : constraints) {
				weights.put(c, c.getWeight());
			}

		} else {
			for (Variable v : variables) {
				if (domainSizes.get(v) != v.getDomainSize()) {
					recompute.add(v);
					for (Variable n : v.getNeighbours()) {
						recompute.add(n);
					}

					domainSizes.put(v, v.getDomainSize());
				}
			}

			for (Constraint c : constraints) {
				if (weights.get(c) != c.getWeight()) {
					for (Variable v : c.getInvolvedVariables()) {
						recompute.add(v);
					}
					weights.put(c, c.getWeight());
				}
			}
		}

		for (Variable v : recompute) {
			if (v.getDomainSize() > 1) {
				for (int i = v.getFirst(); i >= 0; v.getNext(i)) {
					scores.get(v)[i] = getScore(v, i);
				}
			}
		}

		recompute.clear();
	}

	public int selectPair() {
		recompute();
		Variable bestVariable = null;
		int bestValue = -1;
		double bestScore = 0;

		for (Variable v : variables) {
			if (v.getDomainSize() <= 1) {
				continue;
			}
			for (int i = v.getFirst(); i >= 0; v.getNext(i)) {
				final double score = scores.get(v)[i];
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
