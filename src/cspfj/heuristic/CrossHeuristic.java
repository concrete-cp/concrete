package cspfj.heuristic;

import java.util.logging.Logger;

import cspfj.problem.Problem;
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

    public long selectPair(final Problem problem) {
        final Variable bestVariable = variableHeuristic.selectVariable(problem.getVariables()) ;
        return Pair.pair(bestVariable, valueHeuristic.selectIndex(bestVariable), problem);
    }
    
    public void compute() {
    	logger.fine("Initializing heuristics");
    	valueHeuristic.compute() ;
    }
	@Override
	public VariableHeuristic getVariableHeuristic() {
		return variableHeuristic;
	}
    public String toString() {
    	return "Crossed " + variableHeuristic + ", " + valueHeuristic ;
    }
}
