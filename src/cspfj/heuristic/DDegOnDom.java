package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public final class DDegOnDom implements Heuristic {

    private final Problem problem;

    private final Random random;

    private final static TieManager<Variable, Float> tieManager = new TieManager<Variable, Float>(
            null, 0F);

    public DDegOnDom(final Problem prob) {
        this(prob, new Random(0));
    }

    public DDegOnDom(final Problem prob, final Random random) {
        super();
        problem = prob;
        this.random = random;

    }

    public Variable selectVariable() {
        final TieManager<Variable, Float> tieManager = DDegOnDom.tieManager;

        for (Variable v : problem.getVariables()) {
            if (v.isAssigned()) {
                continue;
            }

            if (v.getDomainSize() == 1) { // || !v.isSelectable()) {
                tieManager.newValue(v, 0F, random);
            } else {

                tieManager
                        .newValue(v, -v.getDDeg() / v.getDomainSize(), random);
            }

            // System.out.print(est);
            // System.out.print(" ");
        }
        // System.out.println("Selected : " + bestVariable + "(" + best + ")");

        final Variable bestVariable = tieManager.getBestValue();
        tieManager.clear();

        return bestVariable;
    }

    public int compare(final Variable variable0, final Variable variable1) {
        final float result = variable0.getDDeg() / variable0.getDomainSize()
                - variable1.getDDeg() / variable1.getDomainSize();

        if (result != 0) {
            return result > 0 ? 1 : -1;
        }

        return variable0.getId() - variable1.getId();
    }

}
