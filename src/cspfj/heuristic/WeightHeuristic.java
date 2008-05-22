package cspfj.heuristic;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;

public interface WeightHeuristic extends VariableHeuristic {
	double getWeight(AbstractConstraint constraint);

	void treatConflictConstraint(Constraint constraint);
}
