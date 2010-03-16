package cspfj.generator;

import java.util.Deque;
import java.util.LinkedList;

import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public class ProblemGenerator {

    static {
        try {
            Class.forName("cspfj.constraint.semantic.ReifiedNeq");
            Class.forName("cspfj.constraint.semantic.ReifiedEq");
            Class.forName("cspfj.constraint.semantic.Add");
            Class.forName("cspfj.constraint.semantic.Abs");
            Class.forName("cspfj.constraint.semantic.ReifiedConj");
            Class.forName("cspfj.constraint.semantic.ReifiedDisj");
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException(e);
        }
    }

    public static Problem generate(CSPOM cspom)
            throws FailedGenerationException {
        final Problem problem = new Problem();

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

        problem.prepareVariables();
        problem.prepareConstraints();
        return problem;
    }
}
