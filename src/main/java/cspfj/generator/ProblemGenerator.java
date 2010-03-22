package cspfj.generator;

import java.util.Deque;
import java.util.LinkedList;

import cspfj.exception.FailedGenerationException;
import cspfj.generator.constraint.GeneratorManager;
import cspfj.problem.Problem;
import cspom.CSPOM;
import cspom.compiler.ProblemCompiler;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public class ProblemGenerator {

	public static Problem generate(CSPOM cspom)
			throws FailedGenerationException {

		new ProblemCompiler(cspom).compile();

		final Problem problem = new Problem();

		for (CSPOMVariable v : cspom.getVariables()) {
			problem.addVariable(v);
		}

		final GeneratorManager gm = new GeneratorManager(problem);

		final Deque<CSPOMConstraint> queue = new LinkedList<CSPOMConstraint>(
				cspom.getConstraints());
		CSPOMConstraint firstFailed = null;
		while (!queue.isEmpty()) {
			final CSPOMConstraint constraint = queue.poll();

			if (gm.generate(constraint)) {
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

		problem.prepareVariables();
		problem.prepareConstraints();
		return problem;
	}
}
