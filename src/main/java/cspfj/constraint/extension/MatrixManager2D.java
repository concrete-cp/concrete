package cspfj.constraint.extension;

import cspfj.problem.Variable;
import cspfj.util.BitVector;

public final class MatrixManager2D extends AbstractMatrixManager {

    private static final int MINIMUM_SIZE_FOR_LAST = 3 * Long.SIZE;

    private Matrix2D matrix;

    private final int[][] last;

    private static long checks = 0;

    private static long presenceChecks = 0;

    public MatrixManager2D(final Variable[] scope, final Matrix2D matrix,
            final boolean shared, final int[] tuple) {
        super(scope, matrix, shared, tuple);

        this.matrix = matrix;

        int length = Math.max(scope[0].getDomain().maxSize(), scope[1]
                .getDomain().maxSize());

        /*
         * No need for last data structure if domain sizes <=
         * MINIMUM_SIZE_FOR_LAST
         */
        if (length > MINIMUM_SIZE_FOR_LAST) {
            last = new int[][] { new int[scope[0].getDomain().maxSize()],
                    new int[scope[1].getDomain().maxSize()] };
        } else {
            last = null;
        }

    }

    public static long getChecks() {
        return checks;
    }

    public static long getPresenceChecks() {
        return presenceChecks;
    }

    public static void clearStats() {
        checks = 0;
        presenceChecks = 0;
    }

    public boolean hasSupport(final int variablePosition, final int index) {
        if (last == null) {
            return hasSupportNR(variablePosition, index);
        }
        return hasSupportR(variablePosition, index);
    }

    private boolean hasSupportR(final int variablePosition, final int index) {
        if (controlResidue(variablePosition, index)) {
            return true;
        }

        final BitVector matrixBV = matrix.getBitVector(variablePosition, index);
        final int intersection = matrixBV
                .intersects(variables[1 - variablePosition].getBitDomain());

        if (intersection >= 0) {
            checks += 1 + intersection;
            last[variablePosition][index] = intersection;
            return true;
        }

        checks += matrixBV.realSize();
        return false;
    }

    private boolean hasSupportNR(final int variablePosition, final int index) {
        checks++;
        return matrix.getBitVector(variablePosition, index).intersects(
                variables[1 - variablePosition].getBitDomain()) >= 0;
    }

    public String toString() {
        return "MatrixManager2D\n" + matrix;
    }

    private boolean controlResidue(final int position, final int index) {
        final int part = last[position][index];
        presenceChecks++;
        return part != -1
                && matrix.getBitVector(position, index).intersects(
                        variables[1 - position].getBitDomain(), part);
    }

    @Override
    public Matrix unshareMatrix() {
        matrix = (Matrix2D) super.unshareMatrix();
        return matrix;
    }
}
