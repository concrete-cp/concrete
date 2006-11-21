package cspfj.heuristic;

import java.util.Comparator;

import cspfj.problem.Variable;

public interface VariableHeuristic extends Comparator<Variable> {
	Variable selectVariable() ;
    double getScore(Variable variable);
}
