package cspfj;

import cspfj.constraint.Constraint;

public interface LocalSolver extends Solver {
	double getWeight(Constraint c);
	void increaseWeight(Constraint c);
}
