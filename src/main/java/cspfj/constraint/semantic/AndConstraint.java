package cspfj.constraint.semantic;

import java.util.List;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;

public class AndConstraint extends AbstractAC3Constraint {

	static {
		ConstraintManager.register("and", AndConstraint.class);
	}

	public AndConstraint(Variable result, Variable v0, Variable v1) {
		super(result, v0, v1);
	}

	@Override
	public boolean check() {
		return getValue(0) == conjonction();
	}

	private int conjonction() {
		for (int i = getArity(); --i >= 1;) {
			if (getValue(i) == 0) {
				return 1;
			}
		}
		return 0;
	}

	public static boolean generate(final CSPOMConstraint constraint,
			final Problem problem) {

		final List<Variable> scope = ConstraintManager.getSolverVariables(
				constraint.getScope(), problem);

		for (Variable v : scope) {
			if (v.getDomain() == null) {
				v.setDomain(new BitVectorDomain(0, 1));
			}
		}
		problem.addConstraint(new AndConstraint(scope.get(0), scope.get(1), scope.get(2)));
		return true;
	}
}
