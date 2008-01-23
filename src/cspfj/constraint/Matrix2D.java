package cspfj.constraint;

import cspfj.util.BooleanArray;

public class Matrix2D implements Matrix {
	private final int[][][] matrix;

	public Matrix2D(int x, int y, boolean initialState) {
		final int max = Math.max(x, y);

		matrix = new int[2][max][];

		final int[] xArray = new int[BooleanArray.booleanArraySize(x)];
		BooleanArray.initBooleanArray(xArray, x, initialState);

		final int[] yArray = new int[BooleanArray.booleanArraySize(y)];
		BooleanArray.initBooleanArray(yArray, y, initialState);

		for (int i = max; --i >= 0;) {

			matrix[0][i] = xArray.clone();

			matrix[1][i] = yArray.clone();
		}

	}

	@Override
	public boolean check(final int[] tuple) {
		return BooleanArray.isTrue(matrix[0][tuple[0]], tuple[1]);
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		BooleanArray.set(matrix[0][tuple[0]], tuple[1], status);
		BooleanArray.set(matrix[1][tuple[1]], tuple[0], status);
	}

}
