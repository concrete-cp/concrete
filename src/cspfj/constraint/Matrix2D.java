package cspfj.constraint;

import cspfj.util.BooleanArray;

public class Matrix2D implements Matrix, Cloneable {
	private int[][] xMatrix;
	private int[][] yMatrix;

	public Matrix2D(int x, int y, boolean initialState) {

		xMatrix = new int[x][BooleanArray.booleanArraySize(y)];
		for (int[] m : xMatrix) {
			BooleanArray.initBooleanArray(m, y, initialState);
		}

		yMatrix = new int[y][BooleanArray.booleanArraySize(x)];

		for (int[] m : yMatrix) {
			BooleanArray.initBooleanArray(m, x, initialState);
		}

	}

	@Override
	public boolean check(final int[] tuple) {
		return BooleanArray.isTrue(xMatrix[tuple[0]], tuple[1]);
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		BooleanArray.set(xMatrix[tuple[0]], tuple[1], status);
		BooleanArray.set(yMatrix[tuple[1]], tuple[0], status);
	}

	public int[] getBooleanArray(final int position, final int index) {
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

		final int yLength = xMatrix[0].length;
		matrix2d.xMatrix = new int[xMatrix.length][yLength];
		for (int j = xMatrix.length; --j >= 0;) {
			System.arraycopy(xMatrix[j], 0, matrix2d.xMatrix[j], 0,
					yLength);
		}
		
		final int xLength = yMatrix[0].length;
		matrix2d.yMatrix = new int[yMatrix.length][xLength];
		for (int j = yMatrix.length; --j >= 0;) {
			System.arraycopy(yMatrix[j], 0, matrix2d.yMatrix[j], 0,
					xLength);
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
				stb.append(check(tuple) ? 1 : 0);
			}
			stb.append('\n');
		}
		return stb.toString();
	}

}
