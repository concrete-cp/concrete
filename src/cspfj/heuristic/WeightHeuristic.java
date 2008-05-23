package cspfj.heuristic;

import cspfj.constraint.Constraint;

public interface WeightHeuristic extends VariableHeuristic {
	double getWeight(Constraint arg0);

	void treatConflictConstraint(Constraint constraint);
}
