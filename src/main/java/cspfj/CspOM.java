package cspfj;

import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import cspfj.exception.FailedGenerationException;
import cspfj.generator.Generators;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;
import cspom.variable.Domain;

public class CspOM {
	private final static Generators GENERATORS = new Generators();

	public static Problem generate(cspom.CSPOM cspom)
			throws FailedGenerationException {
		Problem problem = new Problem();

		final Map<CSPOMVariable, Variable> variableMap = new HashMap<CSPOMVariable, Variable>();

		final Deque<CSPOMConstraint> constraintsQueue = new LinkedList<CSPOMConstraint>(
				cspom.getConstraints());

		CSPOMConstraint firstSkippedConstraint = null;

		while (!constraintsQueue.isEmpty()) {
			CSPOMConstraint constraint = constraintsQueue.poll();
			if (GENERATORS.generate(problem, variableMap, constraint)) {
				firstSkippedConstraint = null;
			} else {
				constraintsQueue.offer(constraint);
				if (firstSkippedConstraint == null) {
					firstSkippedConstraint = constraint;
				} else if (firstSkippedConstraint == constraint) {
					throw new FailedGenerationException(
							"Could not generate constraints: "
									+ constraintsQueue);
				}
			}
		}

		return problem;
	}

	private Variable var(CSPOMVariable cspomV, Problem problem,
			Map<CSPOMVariable, Variable> map) {
		Variable var = map.get(cspomV);
		if (var == null) {
			var = new Variable(domain(cspomV.getDomain()));
			problem.addVariable(var);
			map.put(cspomV, var);
		}
		return var;
	}

	private static int[] domain(Domain cspomdomain) {
		final int[] domain = new int[cspomdomain.getNbValues()];
		for (int i = domain.length; --i >= 0;) {
			domain[i] = (Integer) cspomdomain.getValues().get(i);
		}
		return domain;
	}

}
