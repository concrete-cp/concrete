package cspfj.constraint;

import cspfj.util.BooleanArray;

public class Matrix2D implements Matrix {
	private final int[][][] matrix;

	private final int x, y;

	public Matrix2D(int x, int y, boolean initialState) {
		this.x = x;
		this.y = y;

		matrix = new int[2][][];

		final int[] xArray = new int[BooleanArray.booleanArraySize(x)];
		BooleanArray.initBooleanArray(xArray, x, initialState);

		final int[] yArray = new int[BooleanArray.booleanArraySize(y)];
		BooleanArray.initBooleanArray(yArray, y, initialState);

		matrix[0] = new int[x][];
		for (int i = x; --i >= 0;) {
			matrix[0][i] = yArray.clone();
		}

		matrix[1] = new int[y][];
		for (int i = y; --i >= 0;) {
			matrix[1][i] = xArray.clone();
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

	public int[] getBooleanArray(final int position, final int index) {
		return matrix[position][index];
	}

	public Matrix2D clone() {
		try {
			return (Matrix2D) super.clone();
		} catch (CloneNotSupportedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();
		final int[] tuple = new int[2];
		for (int i = 0; i < x; i++) {
			tuple[0] = i;
			for (int j = 0; j < y; j++) {
				tuple[1] = j;
				stb.append(check(tuple) ? 1 : 0);
			}
			stb.append('\n');
		}
		return stb.toString();
	}

}
