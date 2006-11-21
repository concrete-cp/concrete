package cspfj.heuristic;

import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.constraint.Constraint;
import cspfj.constraint.MatrixManager;

public class JW extends AbstractStaticValueHeuristic {

	final private double scores[][];

	final private static Logger logger = Logger.getLogger("cspfj.heuristic.JW");

	final private Constraint[] constraints;

	public JW(Problem problem) {
		super(problem);

		this.constraints = problem.getConstraints();

		scores = new double[problem.getMaxVId() + 1][];

		for (Variable v : problem.getVariables()) {
			scores[v.getId()] = new double[v.getDomain().length];
		}
	}

	public final void compute() {
		for (Variable v : variables) {
			final double[] score = scores[v.getId()];

			for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
				score[i] += w(v.getDomainSize());
			}
		}

		for (Constraint c : constraints) {
			for (Variable v : c.getInvolvedVariables()) {
				final double[] score = scores[v.getId()];

				for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
					// System.out.print(v + "," + i + ":");
					// System.out
					// .print(" + 2^(-1-" + supports.get(v.getId())[i] + ")");
					final int[] tuple = c.getTuple();
					final int position = c.getPosition(v);
					final int arity = c.getArity();

					double tot = w(1 + c.getNbSupports(v, i));

					if (c.getArity() <= Constraint.MAX_ARITY) {
						final MatrixManager matrix = c.getMatrix();
						matrix.setFirstTuple(position, i);

						do {
							if (!c.check()) {
								continue;
							}
							for (int j = arity; --j >= 0;) {
								if (j != position) {
									tot += w(1 + c.getNbSupports(c
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
					score[i] += tot * c.getWeight();
					// System.err.println(c + " : " + v + ", " + i + " : "
					// + tot);
				}
				// System.out.println("=" + score[i]);

			}
		}

		super.compute();
		// for (Variable v: variables) {
		// System.out.print(v+" : ");
		// for (int i = v.getFirstHeuristic(); i >= 0; i =
		// v.getNextHeuristic(i)) {
		// System.out.print(i+" ");
		// }
		// System.out.println() ;
		// }

	}

	protected double w(final int supports) {
		return Math.pow(2, -supports);
	}

	public final double getScore(final Variable variable, final int index) {
		return scores[variable.getId()][index];
	}

}
