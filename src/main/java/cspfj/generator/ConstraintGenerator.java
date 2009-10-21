package cspfj.generator;

import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public interface ConstraintGenerator {
	Constraint generate(Map<CSPOMVariable, Variable> variableMap,
			CSPOMConstraint constraint);

	String getDescription();

	boolean isGenerable(CSPOMConstraint constraint);
}
