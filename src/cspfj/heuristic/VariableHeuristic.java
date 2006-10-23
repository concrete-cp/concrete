package cspfj.heuristic;

import java.util.Comparator;

import cspfj.problem.Variable;

public interface VariableHeuristic extends Comparator<Variable> {
	Variable selectVariable() ;
    float getScore(Variable variable);
    void compute() ;
}
