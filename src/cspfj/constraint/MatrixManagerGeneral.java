package cspfj.constraint;

import cspfj.problem.Variable;

public class MatrixManagerGeneral extends AbstractMatrixManager {

	public MatrixManagerGeneral(Variable[] scope, Matrix matrix) {
		super(scope, matrix);
	}

	

//	@Override
//	public void init(final boolean initialState) throws MatrixTooBigException {
//		BigInteger nbValues = BigInteger.ONE;
//		for (int size : domainSize) {
//			nbValues = nbValues.multiply(BigInteger.valueOf(size));
//			if (nbValues.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
//				throw new MatrixTooBigException();
//			}
//		}
//
//		try {
//			matrix = new boolean[nbValues.intValue()];
//		} catch (OutOfMemoryError e) {
//			throw new MatrixTooBigException(e);
//		}
//
//		Arrays.fill(matrix, initialState);
//	}

	

//	@Override
//	public void intersect(final Variable[] scope, final boolean supports,
//			final int[][] tuples) throws MatrixTooBigException {
//
//		final boolean[] matrix = this.matrix.clone();
//
//		generate(scope, supports, tuples, arity);
//
//		if (matrix != null) {
//			for (int i = matrix.length; --i >= 0;) {
//				this.matrix[i] &= matrix[i];
//			}
//		}
//	}
}
