package cspfj.constraint;

import java.math.BigInteger;
import java.util.Arrays;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public class MatrixManagerGeneral extends MatrixManager {

	private final int arity;

	private boolean[] matrix = null;

	private int variablePosition;

	public MatrixManagerGeneral(Variable[] scope, int[] tuple) {
		super(scope, tuple);
		arity = scope.length;

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
		super.init(initialState);
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
			throw new MatrixTooBigException();
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

	public boolean isTrue(final int[] tuple) {
		return super.isTrue(tuple) || matrix[matrixIndex(tuple)];
	}

	public void intersect(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples) throws MatrixTooBigException {

		final boolean[] matrix;
		if (isActive()) {
			matrix = this.matrix.clone();
		} else {
			matrix = null;
		}

		generate(scope, constraintScope, supports, tuples, arity);

		if (matrix != null) {
			for (int i = matrix.length; --i >= 0;) {
				this.matrix[i] &= matrix[i];
			}
		}
	}

	@Override
	public boolean setFirstTuple(final int variablePosition, final int index) {
		for (int position = arity; --position >= 0;) {
			if (position == variablePosition) {
				tuple[position] = index;
			} else {
				tuple[position] = variables[position].getFirst();
			}
		}

		if (!isTrue(tuple)) {
			return next();
		}

		return true;
	}

	@Override
	public boolean next() {
		if (!setNextTuple()) {
			return false;
		}

		while (!isTrue(tuple)) {
			if (!next()) {
				return false;
			}
		}

		return true;
	}

	private boolean setNextTuple() {
		final int[] tuple = this.tuple;

		final Variable[] involvedVariables = this.variables;
		for (int i = arity; --i >= 0;) {
			if (i == variablePosition) {
				continue;
			}

			final int index = involvedVariables[i].getNext(tuple[i]);

			if (index < 0) {
				tuple[i] = involvedVariables[i].getFirst();
			} else {
				tuple[i] = index;
				return true;
			}
		}
		return false;

	}
}
