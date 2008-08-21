package cspfj.filter;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;

public interface RevisionHandler {
	void revised(final Constraint constraint, final Variable variable);
}
