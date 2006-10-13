package cspfj.heuristic;

import java.util.HashMap;
import java.util.Map;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.constraint.Constraint;

public final class JW extends AbstractValueHeuristic {

    final private Map<Integer, float[]> scores;

    public JW(Problem problem) {
        super(problem);
        final Map<Integer, int[]> supports = new HashMap<Integer, int[]>();
        scores = new HashMap<Integer, float[]>();

        for (Variable v : variables) {
            final int[] support = new int[v.getDomain().length];
            supports.put(v.getId(), support);
            for (int i : v) {
                support[i] = 0;
                for (Constraint c : v.getInvolvingConstraints()) {
                    support[i] += c.getNbTuples(v, i);
                }
                // System.out.println(v+","+i+":"+support[i]);
            }
        }

        for (Variable v : variables) {
            final float[] score = new float[v.getDomain().length];
            scores.put(v.getId(), score);
            for (int i : v) {
                // System.out.print(v + "," + i + ":");
                score[i] = (float)Math.pow(5, -1 - supports.get(v.getId())[i]);
                // System.out
                // .print(" + 2^(-1-" + supports.get(v.getId())[i] + ")");
                for (Constraint c : v.getInvolvingConstraints()) {
                    final int[] tuple = c.getTuple();
                    final int position = c.getPosition(v);
                    final int arity = c.getArity();
                    c.setFirstTuple(position, i);
                    do {
                        if (!c.check()) {
                            continue;
                        }
                        for (int j = arity; --j >= 0;) {
                            if (j != position) {
                                score[i] += Math
                                        .pow(
                                                2,
                                                -1
                                                        - supports
                                                                .get(c
                                                                        .getInvolvedVariables()[j]
                                                                        .getId())[tuple[j]]);
                                // System.out
                                // .print(" + 2^(-1-"
                                // + supports
                                // .get(c
                                // .getInvolvedVariables()[j]
                                // .getId())[j]
                                // + ")");
                            }
                        }
                    } while (c.setNextTuple(position));
                }
                // System.out.println("=" + score[i]);
            }
        }

    }

    public float getScore(final Variable variable, final int index) {
        return -scores.get(variable.getId())[index];
    }

}
