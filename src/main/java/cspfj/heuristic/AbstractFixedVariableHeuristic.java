package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractFixedVariableHeuristic implements
        VariableHeuristic {

    private final Problem problem;

    public AbstractFixedVariableHeuristic(final Problem problem) {
        this.problem = problem;
    }

    @Override
    public final Variable selectVariable() {

        Variable bestVar = null;
        double bestScore = Double.NEGATIVE_INFINITY;

        for (Variable v : problem.getVariables()) {
            if (v.getDomainSize() == 1) {
                continue;
            }
            final double score = getScore(v);
            if (score > bestScore) {
                bestVar = v;
                bestScore = score;
            }
        }

        return bestVar;
    }

    public final int compare(final Variable variable1, final Variable variable2) {
        return Double.compare(getScore(variable1), getScore(variable2));
    }
}
