package cspfj.heuristic;

import cspfj.problem.Variable;

public final class CrossHeuristic implements Heuristic {

    private final VariableHeuristic variableHeuristic;

    private final ValueHeuristic valueHeuristic;

    public CrossHeuristic(VariableHeuristic variableHeuristic,
            ValueHeuristic valueHeuristic) {
        this.variableHeuristic = variableHeuristic;
        this.valueHeuristic = valueHeuristic;
    }

    public Pair selectPair() {
        final Variable bestVariable = variableHeuristic.selectVariable() ;
        return new Pair(bestVariable, valueHeuristic.selectIndex(bestVariable));
    }
    
}
