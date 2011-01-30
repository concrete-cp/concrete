package cspfj.generator;

import java.util.Collection;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

import com.google.common.primitives.Ints;

import cspfj.exception.FailedGenerationException;
import cspfj.generator.constraint.GeneratorManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.BooleanDomain;
import cspom.variable.CSPOMDomain;
import cspom.variable.CSPOMVariable;

public final class ProblemGenerator {

    private ProblemGenerator() {
    }

    public static Problem generate(final CSPOM cspom)
            throws FailedGenerationException {

        // new ProblemCompiler(cspom).compile();

        final Problem problem = new Problem();

        for (CSPOMVariable v : cspom.getVariables()) {
            problem.addVariable(v.getName(), generateDomain(v.getDomain()));
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
                        "Could not generate the constraints "
                                + queue.toString());
            } else {
                if (firstFailed == null) {
                    firstFailed = constraint;
                }
                queue.offer(constraint);
            }
        }

        return problem;
    }

    private static Domain generateDomain(final CSPOMDomain<?> cspomDomain) {
        if (cspomDomain == null) {
            return null;
        }
        if (cspomDomain instanceof BooleanDomain) {
            final BooleanDomain bD = (BooleanDomain) cspomDomain;
            if (bD.isConstant()) {
                return new cspfj.problem.BooleanDomain(bD.getBoolean());
            }
            return new cspfj.problem.BooleanDomain();
        }
        return new BitVectorDomain(
                Ints.toArray((Collection<Integer>) cspomDomain.getValues()));
    }
}
