package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;

public class AddConstraint extends AbstractAC3Constraint {

	static {
		ConstraintManager.register("sub", AddConstraint.class);
	}

	public AddConstraint(Variable result, Variable v0, Variable v1) {
		super(result, v0, v1);
	}

	@Override
	public boolean check() {
		return getValue(0) == (getValue(1) + getValue(2));
	}

	public static boolean generate(final CSPOMConstraint constraint,
			final Problem problem) {
		return false;
	}
}
