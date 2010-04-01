package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class CrossHeuristic implements Heuristic {

    private final VariableHeuristic variableHeuristic;

    private final ValueHeuristic valueHeuristic;

    // private final static Logger logger =
    // Logger.getLogger("cspfj.CrossHeuristic");

    public CrossHeuristic(final VariableHeuristic variableHeuristic,
            final ValueHeuristic valueHeuristic) {
        this.variableHeuristic = variableHeuristic;
        this.valueHeuristic = valueHeuristic;
    }

    public Pair selectPair(final Problem problem) {
        final Variable bestVariable = variableHeuristic.selectVariable();
        if (bestVariable == null) {
            return null;
        }
        return new Pair(bestVariable, valueHeuristic.selectIndex(bestVariable));
    }

    public void compute() {
        // logger.fine("Initializing heuristics");
        valueHeuristic.compute();
    }

    public String toString() {
        return "Crossed " + variableHeuristic + ", " + valueHeuristic;
    }
}
