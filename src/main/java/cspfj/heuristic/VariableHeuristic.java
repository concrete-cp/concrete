package cspfj.heuristic;

import java.util.Comparator;

import cspfj.problem.Variable;

public interface VariableHeuristic extends Comparator<Variable> {
	Variable selectVariable(Variable[] variable) ;
    double getScore(Variable variable);
}
