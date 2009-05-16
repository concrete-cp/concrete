package cspfj.filter;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;

public interface RevisionHandler {
	/**
	 * Must be called when the given constraint has removed at least a
	 * value from the domain of the given variable.
	 * 
	 * @param constraint
	 * @param variable
	 */
	void revised(final Constraint constraint, final Variable variable);
}
