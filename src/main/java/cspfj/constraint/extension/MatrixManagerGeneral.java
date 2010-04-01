package cspfj.constraint.extension;

import cspfj.problem.Variable;

public final class MatrixManagerGeneral extends AbstractMatrixManager {
    public MatrixManagerGeneral(Variable[] scope, Matrix matrix,
            final boolean shared) {
        super(scope, matrix, shared);
    }
}
