package cspfj.generator;

import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.variable.CSPOMVariable;
import cspom.variable.UnknownDomain;

public class ReifiedNeqGenerator implements ConstraintGenerator {

	@Override
	public Constraint generate(Map<CSPOMVariable, Variable> variableMap,
			CSPOMConstraint constraint) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getDescription() {
		return "neq";
	}

	@Override
	public boolean isGenerable(CSPOMConstraint constraint) {
		if (!(constraint instanceof FunctionalConstraint)) {
			return false;
		}
		int unk = 0;
		for (CSPOMVariable v : ((FunctionalConstraint) constraint)
				.getArguments()) {
			if (v.getDomain() instanceof UnknownDomain || v.getDomain() instanceof )
		}
	}

}
