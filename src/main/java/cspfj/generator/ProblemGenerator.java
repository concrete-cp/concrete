package cspfj.generator;

import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public class ProblemGenerator {

	static {
		try {
			Class.forName("cspfj.constraint.semantic.ReifiedNeqConstraint");
			Class.forName("cspfj.constraint.semantic.AddConstraint");
			Class.forName("cspfj.constraint.semantic.AbsConstraint");
			Class.forName("cspfj.constraint.semantic.AndConstraint");
		} catch (ClassNotFoundException e) {
			throw new IllegalStateException(e);
		}
	}

	public static Problem generate(CSPOM cspom)
			throws FailedGenerationException {
		final Problem problem = new Problem();

		final Map<CSPOMVariable, Variable> variableMap = new HashMap<CSPOMVariable, Variable>();

		for (CSPOMVariable v : cspom.getVariables()) {
			problem.addVariable(v);
		}

		final Deque<CSPOMConstraint> queue = new LinkedList<CSPOMConstraint>(
				cspom.getConstraints());
		CSPOMConstraint firstFailed = null;
		while (!queue.isEmpty()) {
			final CSPOMConstraint constraint = queue.poll();

			if (ConstraintManager.generate(constraint, problem)) {
				firstFailed = null;
			} else if (firstFailed == constraint) {
				throw new FailedGenerationException(
						"Could not generate the constraints " + queue);
			} else {
				if (firstFailed == null) {
					firstFailed = constraint;
				}
				queue.offer(constraint);
			}
		}
		return problem;
	}
}
