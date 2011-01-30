package cspfj.constraint.extension;

import java.util.Arrays;

public final class MatrixGeneral implements Matrix, Cloneable {
    private int[] skip;

    private boolean[] matrix;

    private boolean empty;

    public MatrixGeneral(final int[] sizes, final boolean initialState) {
        int nbValues = 1;
        for (int size : sizes) {
            nbValues *= size;
        }

        matrix = new boolean[nbValues];
        Arrays.fill(matrix, initialState);

        skip = new int[sizes.length - 1];
        skip[0] = sizes[0];
        for (int i = 1; i < skip.length; i++) {
            skip[i] = skip[i - 1] * sizes[i];
        }
        empty = !initialState;
        // System.out.println(Arrays.toString(skip));
    }

    @Override
    public boolean check(final int[] tuple) {
        return matrix[matrixIndex(tuple)];
    }

    @Override
    public void set(final int[] tuple, final boolean status) {
        matrix[matrixIndex(tuple)] = status;
        empty = false;
    }

    private int matrixIndex(final int[] tuple) {
        int index = tuple[0];
        for (int i = skip.length; --i >= 0;) {
            index += skip[i] * tuple[i + 1];
        }
        return index;
    }

    @Override
    public MatrixGeneral clone() throws CloneNotSupportedException {
        final MatrixGeneral clone = (MatrixGeneral) super.clone();
        clone.skip = skip.clone();
        clone.matrix = this.matrix.clone();
        return clone;

    }

    @Override
    public boolean isEmpty() {
        return empty;
    }

}
