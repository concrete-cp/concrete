package cspfj.constraint;

import java.math.BigInteger;
import java.util.Arrays;

import cspfj.exception.MatrixTooBigException;

public class MatrixGeneral implements Matrix {

	public final int[] sizes;

	public final boolean[] matrix;

	public MatrixGeneral(int[] sizes, boolean initialState)
			throws MatrixTooBigException {
		BigInteger nbValues = BigInteger.ONE;
		for (int size : sizes) {
			nbValues = nbValues.multiply(BigInteger.valueOf(size));
			if (nbValues.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
				throw new MatrixTooBigException();
			}
		}
		this.sizes = sizes;
		matrix = new boolean[nbValues.intValue()];
		Arrays.fill(matrix, initialState);
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
		final int[] domainSize = this.sizes;
		int index = 0;
		for (int i = domainSize.length; --i >= 0;) {
			int skip = 1;
			for (int j = i; --j >= 0;) {
				skip *= domainSize[j];
			}
			index += skip * tuple[i];
		}
		return index;
	}

}
