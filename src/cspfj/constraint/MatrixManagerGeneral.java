package cspfj.constraint;

import java.math.BigInteger;
import java.util.Arrays;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public class MatrixManagerGeneral extends AbstractMatrixManager {
	private boolean[] matrix = null;

	public MatrixManagerGeneral(Variable[] scope) {
		super(scope);
	}

	private int matrixIndex(final int[] tuple) {
		final int[] domainSize = this.domainSize;
		int index = 0;
		for (int i = this.domainSize.length; --i >= 0;) {
			int skip = 1;
			for (int j = i; --j >= 0;) {
				skip *= domainSize[j];
			}
			index += skip * tuple[i];
		}
		return index;
	}

	@Override
	public void init(final boolean initialState) throws MatrixTooBigException {
		BigInteger nbValues = BigInteger.ONE;
		for (int size : domainSize) {
			nbValues = nbValues.multiply(BigInteger.valueOf(size));
			if (nbValues.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
				throw new MatrixTooBigException();
			}
		}

		try {
			matrix = new boolean[nbValues.intValue()];
		} catch (OutOfMemoryError e) {
			throw new MatrixTooBigException(e);
		}

		Arrays.fill(matrix, initialState);
	}

	@Override
	public boolean set(final int[] tuple, final boolean status) {
		final boolean[] matrix = this.matrix;
		final int matrixIndex = matrixIndex(tuple);

		if (matrix[matrixIndex] == status) {
			return false;
		}
		matrix[matrixIndex] ^= true;
		return true;
	}

	@Override
	public boolean isTrue(final int[] tuple) {
		return matrix[matrixIndex(tuple)];
	}

	@Override
	public boolean check() {
		checks++;
		return matrix[matrixIndex(tuple)];
	}

	@Override
	public void intersect(final Variable[] scope, final boolean supports,
			final int[][] tuples) throws MatrixTooBigException {

		final boolean[] matrix = this.matrix.clone();

		generate(scope, supports, tuples, arity);

		if (matrix != null) {
			for (int i = matrix.length; --i >= 0;) {
				this.matrix[i] &= matrix[i];
			}
		}
	}
}
