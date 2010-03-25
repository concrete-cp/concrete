package cspfj.generator.constraint;

import cspfj.exception.FailedGenerationException;
import cspom.constraint.CSPOMConstraint;

public interface Generator {
    boolean generate(CSPOMConstraint<?> constraint)
            throws FailedGenerationException;
}
