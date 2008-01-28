package cspfj.constraint;

import java.math.BigInteger;
import java.util.Arrays;

import cspfj.exception.MatrixTooBigException;

public class MatrixGeneral implements Matrix, Cloneable {

	public int[] skip;

	public boolean[] matrix;

	public MatrixGeneral(int[] sizes, boolean initialState)
			throws MatrixTooBigException {
		BigInteger nbValues = BigInteger.ONE;
		for (int size : sizes) {
			nbValues = nbValues.multiply(BigInteger.valueOf(size));
			if (nbValues.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
				throw new MatrixTooBigException();
			}
		}

		matrix = new boolean[nbValues.intValue()];
		Arrays.fill(matrix, initialState);

		skip = new int[sizes.length];
		skip[0] = 1;
		for (int i = 1; i < sizes.length; i++) {
			skip[i] = skip[i - 1] * sizes[i - 1];
		}

		// System.out.println(Arrays.toString(skip));
	}

	@Override
	public boolean check(final int[] tuple) {
		return matrix[matrixIndex(tuple)];
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		matrix[matrixIndex(tuple)] = status;
	}

	private int matrixIndex(final int[] tuple) {
		final int[] skip = this.skip;
		int index = 0;
		for (int i = skip.length; --i >= 0;) {
			index += skip[i] * tuple[i];
		}
		return index;
	}

	public MatrixGeneral clone() throws CloneNotSupportedException {
		final MatrixGeneral matrix = (MatrixGeneral) super.clone();
		matrix.skip = skip.clone();
		matrix.matrix = this.matrix.clone();
		return matrix;

	}

}