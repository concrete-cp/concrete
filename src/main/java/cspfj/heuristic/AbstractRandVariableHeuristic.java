package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public abstract class AbstractRandVariableHeuristic implements
        VariableHeuristic {

    private static final Random RAND = new Random(0);

    private final Problem problem;

    private final TieManager<Variable> tieManager;

    public AbstractRandVariableHeuristic(final Problem problem) {
        this.problem = problem;
        tieManager = new TieManager<Variable>(RAND);
        RAND.setSeed(0);
    }

    @Override
    public final Variable selectVariable() {
        tieManager.clear();

        for (Variable v : problem.getVariables()) {
            if (v.dom().size() > 1) {
                tieManager.newValue(v, getScore(v));
            }
        }

        return tieManager.getBestValue();
    }

    public final int compare(final Variable variable1, final Variable variable2) {
        return Double.compare(getScore(variable1), getScore(variable2));
    }

}
