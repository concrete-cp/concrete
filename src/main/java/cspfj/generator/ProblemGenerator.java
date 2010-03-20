package cspfj.generator;

import java.util.Deque;
import java.util.LinkedList;

import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspom.CSPOM;
import cspom.compiler.ProblemCompiler;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public class ProblemGenerator {

    static {
        try {
            Class.forName("cspfj.constraint.semantic.ReifiedNeq");
            Class.forName("cspfj.constraint.semantic.Neq");
            Class.forName("cspfj.constraint.semantic.ReifiedEq");
            Class.forName("cspfj.constraint.semantic.Eq");
            Class.forName("cspfj.constraint.semantic.Add");
            Class.forName("cspfj.constraint.semantic.Mul");
            Class.forName("cspfj.constraint.semantic.Abs");
            Class.forName("cspfj.constraint.semantic.Gt");
            Class.forName("cspfj.constraint.semantic.ReifiedGt");
            Class.forName("cspfj.constraint.semantic.Disj");
            Class.forName("cspfj.constraint.extension.AbstractExtensionConstraint");
            Class.forName("cspfj.constraint.semantic.AllDifferent");
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException(e);
        }
    }

    public static Problem generate(CSPOM cspom)
            throws FailedGenerationException {
        
        new ProblemCompiler(cspom).compile();
        
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
        ExtensionGenerator.clear();
        return problem;
    }
}
