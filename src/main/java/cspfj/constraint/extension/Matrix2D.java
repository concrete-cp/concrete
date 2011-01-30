package cspfj.constraint.extension;

import cspfj.util.BitVector;

public final class Matrix2D implements Matrix {
    private BitVector[] xMatrix;
    private BitVector[] yMatrix;
    private boolean empty;

    public Matrix2D(final int xSize, final int ySize, final boolean initialState) {

        xMatrix = new BitVector[xSize];
        for (int i = xMatrix.length; --i >= 0;) {
            xMatrix[i] = BitVector.factory(ySize, initialState);
        }

        yMatrix = new BitVector[ySize];

        for (int i = yMatrix.length; --i >= 0;) {
            yMatrix[i] = BitVector.factory(xSize, initialState);
        }
        empty = initialState;
    }

    @Override
    public boolean check(final int[] tuple) {
        return xMatrix[tuple[0]].get(tuple[1]);
    }

    @Override
    public void set(final int[] tuple, final boolean status) {
        xMatrix[tuple[0]].set(tuple[1], status);
        yMatrix[tuple[1]].set(tuple[0], status);
        empty = false;
    }

    public BitVector getBitVector(final int position, final int index) {
        switch (position) {
        case 0:
            return xMatrix[index];
        case 1:
            return yMatrix[index];
        default:
            throw new IllegalArgumentException();
        }
    }

    public Matrix2D clone() throws CloneNotSupportedException {
        final Matrix2D matrix2d = (Matrix2D) super.clone();

        matrix2d.xMatrix = new BitVector[xMatrix.length];

        for (int j = xMatrix.length; --j >= 0;) {
            matrix2d.xMatrix[j] = xMatrix[j].clone();
        }

        matrix2d.yMatrix = new BitVector[yMatrix.length];
        for (int j = yMatrix.length; --j >= 0;) {
            matrix2d.yMatrix[j] = yMatrix[j].clone();
        }

        return matrix2d;

    }

    public String toString() {
        final StringBuilder stb = new StringBuilder();
        final int[] tuple = new int[2];
        for (int i = 0; i < xMatrix.length; i++) {
            tuple[0] = i;
            for (int j = 0; j < yMatrix.length; j++) {
                tuple[1] = j;
                if (check(tuple)) {
                    stb.append(1);
                } else {
                    stb.append(0);
                }
            }
            stb.append('\n');
        }
        return stb.toString();
    }

    public boolean isEmpty() {
        return empty;
    }

}
