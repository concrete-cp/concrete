package cspfj.heuristic;

import java.util.HashMap;
import java.util.Map;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.constraint.Constraint;
import cspfj.constraint.MatrixManager;

public final class JWs extends AbstractValueHeuristic {

	final private Map<Integer, float[]> scores;

	final private Constraint[] constraints;

	public JWs(Problem problem) {
		super(problem);
		constraints = problem.getConstraints();
		scores = new HashMap<Integer, float[]>();
	}

	public void compute() {
		for (Constraint c : constraints) {
			for (Variable v : c.getInvolvedVariables()) {
				float[] score = scores.get(v.getId());
				if (score == null) {
					score = new float[v.getDomain().length];
					scores.put(v.getId(), score);
				}
				for (int i = v.getFirst(); i >= 0; v.getNext(i)) {
					// System.out.print(v + "," + i + ":");
					// System.out
					// .print(" + 2^(-1-" + supports.get(v.getId())[i] + ")");
					final int[] tuple = c.getTuple();
					final int position = c.getPosition(v);
					final int arity = c.getArity();
					final MatrixManager matrix = c.getMatrix();
					matrix.setFirstTuple(position, i);
					int tot = c.getNbSupports(v, i);
					do {
						if (!c.check()) {
							continue;
						}
						for (int j = arity; --j >= 0;) {
							if (j != position) {
								tot += c.getNbSupports(
										c.getInvolvedVariables()[j], tuple[j]);
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
					score[i] += tot;
					// System.err.println(c + " : " + v + ", " + i + " : "
					// + tot);
				}
				// System.out.println("=" + score[i]);

			}
		}

	}

	public float getScore(final Variable variable, final int index) {
		return 1 / scores.get(variable.getId())[index];
	}
}
