package cspfj.heuristic;

import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.constraint.Constraint;
import cspfj.constraint.MatrixManager;

public class JW extends AbstractValueHeuristic {

	final private float[][] scores;

	final private Constraint[] constraints;
	
	final private static Logger logger = Logger.getLogger("cspfj.heuristic.JW");

	public JW(Problem problem) {
		super(problem);
		constraints = problem.getConstraints();
		scores = new float[problem.getMaxVId() + 1][];
		for (Variable v : problem.getVariables()) {
			scores[v.getId()] = new float[v.getDomain().length];
		}
	}

	public final void compute() {
		logger.info("Computing JW") ;
		for (Constraint c : constraints) {
			for (Variable v : c.getInvolvedVariables()) {
				final float[] score = scores[v.getId()];

				for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
					// System.out.print(v + "," + i + ":");
					// System.out
					// .print(" + 2^(-1-" + supports.get(v.getId())[i] + ")");
					final int[] tuple = c.getTuple();
					final int position = c.getPosition(v);
					final int arity = c.getArity();

					double tot = w(c.getNbSupports(v, i));

					if (c.getArity() <= Constraint.MAX_ARITY) {
						final MatrixManager matrix = c.getMatrix();
						matrix.setFirstTuple(position, i);

						do {
							if (!c.check()) {
								continue;
							}
							for (int j = arity; --j >= 0;) {
								if (j != position) {
									tot += w(c.getNbSupports(c
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
					score[i] += tot;
					// System.err.println(c + " : " + v + ", " + i + " : "
					// + tot);
				}
				// System.out.println("=" + score[i]);

			}
		}

	}

	protected static double w(int supports) {
		return Math.pow(2, -1 - supports);
	}

	public final float getScore(final Variable variable, final int index) {
		return scores[variable.getId()][index];
	}
	
}
