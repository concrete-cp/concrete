package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public abstract class AbstractVariableHeuristic implements VariableHeuristic {

    private static final Random RAND = new Random(0);

    protected final Problem problem;

    private final TieManager<Variable> tieManager;

    public AbstractVariableHeuristic(final Problem problem) {
        this.problem = problem;
        tieManager = new TieManager<Variable>(RAND);
    }

    @Override
    public final Variable selectVariable() {
        tieManager.clear();

        for (Variable v : problem.getVariables()) {
            if (v.getDomainSize() > 1) {
                tieManager.newValue(v, getScore(v));
            }
        }

        if (tieManager.getBestValue() == null) {
            for (Variable v : problem.getVariables()) {
                if (!v.isAssigned()) {
                    return v;
                }
            }
        }

        return tieManager.getBestValue();
    }

    public final int compare(final Variable variable1, final Variable variable2) {
        return Double.compare(getScore(variable1), getScore(variable2));
    }

    private static double poisson(final double moy) {
        return -moy * Math.log(RAND.nextDouble());
    }

}
