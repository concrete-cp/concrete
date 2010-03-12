package cspfj.heuristic;

import java.util.Comparator;

import cspfj.problem.IntVariable;

public interface VariableHeuristic extends Comparator<IntVariable> {
	IntVariable selectVariable(IntVariable[] variable) ;
    double getScore(IntVariable variable);
}
