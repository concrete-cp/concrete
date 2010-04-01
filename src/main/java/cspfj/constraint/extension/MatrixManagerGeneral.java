package cspfj.constraint.extension;

import cspfj.problem.Variable;

public final class MatrixManagerGeneral extends AbstractMatrixManager {
    public MatrixManagerGeneral(final Variable[] scope, final Matrix matrix,
            final boolean shared, final int[] tuple) {
        super(scope, matrix, shared, tuple);
    }
}
