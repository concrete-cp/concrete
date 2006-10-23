package cspfj.heuristic;

import java.util.logging.Logger;

import cspfj.problem.Variable;

public final class CrossHeuristic implements Heuristic {

    private final VariableHeuristic variableHeuristic;

    private final ValueHeuristic valueHeuristic;
    
    private final static Logger logger = Logger.getLogger("cspfj.CrossHeuristic");

    public CrossHeuristic(VariableHeuristic variableHeuristic,
            ValueHeuristic valueHeuristic) {
        this.variableHeuristic = variableHeuristic;
        this.valueHeuristic = valueHeuristic;
    }

    public Pair selectPair() {
        final Variable bestVariable = variableHeuristic.selectVariable() ;
        return new Pair(bestVariable, valueHeuristic.selectIndex(bestVariable));
    }
    
    public void compute() {
    	logger.fine("Initializing heuristics");
    	variableHeuristic.compute() ;
    	valueHeuristic.compute() ;
    }
    
}
