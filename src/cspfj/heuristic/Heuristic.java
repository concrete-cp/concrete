package cspfj.heuristic;

import cspfj.problem.Variable;

public class Heuristic {

	private final VariableHeuristic variableHeuristic;

	private final ValueHeuristic valueHeuristic;

	public Heuristic(VariableHeuristic variableHeuristic,
			ValueHeuristic valueHeuristic) {
		this.variableHeuristic = variableHeuristic;
		this.valueHeuristic = valueHeuristic;
	}

	public Pair selectPair() {
		final Variable variable = variableHeuristic.selectVariable();
		return new Pair(variable, valueHeuristic.selectIndex(variable));
	}


}
