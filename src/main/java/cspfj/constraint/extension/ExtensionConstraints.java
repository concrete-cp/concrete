package cspfj.constraint.extension;

import cspfj.exception.FailedGenerationException;
import cspfj.problem.Variable;

public final class ExtensionConstraints {
    private ExtensionConstraints() {
    }

    public static ExtensionConstraint newExtensionConstraint(
            final Matrix matrix, final Variable... scope)
            throws FailedGenerationException {
        final ExtensionConstraint gConstraint;
        if (matrix instanceof Matrix2D) {
            gConstraint = new ExtensionConstraint2D(scope, (Matrix2D) matrix,
                    true);
        } else if (matrix instanceof TupleSet) {
            gConstraint = new ExtensionConstraintDynamic(scope,
                    (TupleSet) matrix, true);
        } else {
            gConstraint = new ExtensionConstraintGeneral(matrix, true, scope);
        }
        return gConstraint;
    }
}
