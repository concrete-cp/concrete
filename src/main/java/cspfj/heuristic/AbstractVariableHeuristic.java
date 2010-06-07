package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public abstract class AbstractVariableHeuristic implements VariableHeuristic {

    private static final Random RAND = new Random(0);

    private final Problem problem;

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

        return tieManager.getBestValue();
    }

//    @Override
//    public final Variable selectVariable() {
//
//        Variable bestVar = null;
//        double bestScore = Double.NEGATIVE_INFINITY;
//
//        for (Variable v : problem.getVariables()) {
//            if (v.getDomainSize() == 1) {
//                continue;
//            }
//            final double score = getScore(v);
//            if (score > bestScore) {
//                bestVar = v;
//                bestScore = score;
//            }
//        }
//
//        return bestVar;
//    }

    public final int compare(final Variable variable1, final Variable variable2) {
        return Double.compare(getScore(variable1), getScore(variable2));
    }

    private static double poisson(final double moy) {
        return -moy * Math.log(RAND.nextDouble());
    }

}
