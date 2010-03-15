package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;

public class AbsConstraint extends AbstractAC3Constraint {

	static {
		ConstraintManager.register("abs", AbsConstraint.class);
	}

	public AbsConstraint(Variable result, Variable v0) {
		super(result, v0);
	}

	@Override
	public boolean check() {
		return getValue(0) == (Math.abs(getValue(1)));
	}

	public static boolean generate(final CSPOMConstraint constraint,
			final Problem problem) {
		return false;
	}
}
