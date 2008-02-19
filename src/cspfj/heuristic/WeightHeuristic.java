package cspfj.heuristic;

import cspfj.constraint.Constraint;

public interface WeightHeuristic extends VariableHeuristic {
	double getWeight(Constraint constraint);

	void treatConflictConstraint(Constraint constraint);
}
