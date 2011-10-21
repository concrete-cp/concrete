package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class ProdHeuristic implements Heuristic {

    private final VariableHeuristic variableHeuristic;

    private final ValueHeuristic valueHeuristic;

    public ProdHeuristic(final VariableHeuristic variableHeuristic,
            final ValueHeuristic valueHeuristic) {
        this.variableHeuristic = variableHeuristic;
        this.valueHeuristic = valueHeuristic;
    }

    public Pair selectPair(final Problem problem) {
        Variable bestVariable = null;
        for (Variable v : problem.getVariables()) {
            if (bestVariable == null
                    || (v.dom().size() != 1 && compare(v, bestVariable) > 0)) {
                bestVariable = v;
            }
        }

        return new Pair(bestVariable, valueHeuristic.selectIndex(bestVariable));
    }

    public int compare(final Variable variable1, final Variable variable2) {

        final int result = Double.compare(
                variableHeuristic.getScore(variable1)
                        * valueHeuristic.getScore(variable1,
                                valueHeuristic.selectIndex(variable1)),
                variableHeuristic.getScore(variable2)
                        * valueHeuristic.getScore(variable2,
                                valueHeuristic.selectIndex(variable2)));

        if (result == 0) {
            return variable1.getId() - variable2.getId();
        }

        return result;
    }

    public void compute() {
        valueHeuristic.compute();
    }

    public String toString() {
        return variableHeuristic + " * " + valueHeuristic;
    }
}
