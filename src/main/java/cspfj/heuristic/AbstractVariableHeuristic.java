package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractVariableHeuristic implements VariableHeuristic {

    private static final Random RAND = new Random(0);

    protected final Problem problem;

    public AbstractVariableHeuristic(final Problem problem) {
        this.problem = problem;
    }

    public final Variable selectVariable(final Variable[] variables) {
        int ties = 1;
        Variable bestVariable = null;
        double bestScore = Double.MIN_VALUE;
        // int i = variables.length;
        // while (--i >= 0) {
        // if (!variables[i].isAssigned()) {
        // bestVariable = variables[i];
        // bestScore = getScore(bestVariable);
        // break;
        // }
        // }

        for (Variable v : variables) {
            if (v.getDomainSize() > 1) {
                final double score = getScore(v);
                final int comp = Double.compare(score, bestScore);
                if (comp > 0 || (comp == 0 && ++ties * RAND.nextFloat() > 1)) {
                    bestVariable = v;
                    bestScore = score;
                    ties = 1;
                }
            }
        }

        if (bestVariable == null) {
            for (Variable v : variables) {
                if (!v.isAssigned()) {
                    return v;
                }
            }
        }

        return bestVariable;
    }

    public final int compare(final Variable variable1, final Variable variable2) {
        // if (variable1 == null) {
        // return 1;
        // }
        // if (variable2 == null) {
        // return -1;
        // }
        return Double.compare(getScore(variable1), getScore(variable2));
    }

    private static double poisson(final double moy) {
        return -moy * Math.log(RAND.nextDouble());
    }

}
