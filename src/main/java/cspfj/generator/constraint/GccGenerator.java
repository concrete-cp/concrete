package cspfj.generator.constraint;

import cspfj.constraint.semantic.Gcc;
import cspfj.constraint.semantic.Gcc.Bounds;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public class GccGenerator extends AbstractGenerator {

    public GccGenerator(Problem problem) {
        super(problem);
    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        if (!(constraint instanceof GeneralConstraint)) {
            throw new FailedGenerationException(constraint
                    + " is not supported");
        }

        final Variable[] solverVariables = getSolverVariables(constraint
                .getScope());
        if (nullVariable(solverVariables) != null) {
            return false;
        }

        final String[] params = constraint.getParameters().split(", +");
        if (params.length % 3 != 0) {
            throw new FailedGenerationException("3 parameters per value");
        }

        final Bounds[] bounds = new Bounds[params.length / 3];
        for (int i = 0; i < params.length / 3; i++) {
            bounds[i] = new Bounds(Integer.valueOf(params[3 * i]),
                    Integer.valueOf(params[3 * i + 1]),
                    Integer.valueOf(params[3 * i + 2]));
        }

        addConstraint(new Gcc(solverVariables, bounds));
        return true;
    }

}
