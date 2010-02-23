package cspfj;

import java.lang.reflect.InvocationTargetException;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.generator.ConstraintManager;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public class CspOM {
	private final Problem problem;
	private final Map<CSPOMVariable, Variable> variableMap = new HashMap<CSPOMVariable, Variable>();

	private CspOM(final Problem problem) {
		this.problem = problem;
	}

	public static Problem generate(cspom.CSPOM cspom)
			throws FailedGenerationException {
		final CspOM generator = new CspOM(new Problem());
		generator.gen(cspom);
		return generator.problem;
	}

	private void gen(cspom.CSPOM cspom) throws FailedGenerationException {
		final Deque<CSPOMConstraint> constraintsQueue = new LinkedList<CSPOMConstraint>(
				cspom.getConstraints());

		CSPOMConstraint firstSkippedConstraint = null;

		while (!constraintsQueue.isEmpty()) {
			CSPOMConstraint cspomConstraint = constraintsQueue.poll();
			Constraint cspfjConstraint;
			try {
				cspfjConstraint = ConstraintManager.generate(cspomConstraint
						.getDescription(), obtainScope(cspomConstraint));
			} catch (Exception e) {
				throw new FailedGenerationException(e);
			}

			if (cspfjConstraint == null) {
				constraintsQueue.offer(cspomConstraint);
				if (firstSkippedConstraint == null) {
					firstSkippedConstraint = cspomConstraint;
				} else if (firstSkippedConstraint == cspomConstraint) {
					throw new FailedGenerationException(
							"Could not generate constraints: "
									+ constraintsQueue);
				}
			} else {
				firstSkippedConstraint = null;
				problem.addConstraint(cspfjConstraint);
			}
		}
	}

	private Variable[] obtainScope(CSPOMConstraint constraint) {
		final Variable[] scope = new Variable[constraint.getArity()];
		for (int i = constraint.getArity(); --i >= 0;) {
			scope[i] = variableMap.get(constraint.getScope().get(i));
		}
		return scope;
	}

	private Variable var(CSPOMVariable cspomV, Problem problem,
			Map<CSPOMVariable, Variable> map) {
		Variable var = map.get(cspomV);
		if (var == null) {
			var = new Variable(null);
			problem.addVariable(var);
			map.put(cspomV, var);
		}
		return var;
	}

}
