package cspfj.constraint;

import java.math.BigInteger;
import java.util.Arrays;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public class MatrixManager3D extends MatrixManager {

	private final int arity;

	private boolean[][][] matrix = null;

	private int variablePosition;

	public MatrixManager3D(Variable[] scope, int[] tuple) {
		super(scope, tuple);
		arity = scope.length;

	}

	@Override
	public void init(final boolean initialState) throws MatrixTooBigException {
		try {
			matrix = new boolean[domainSize[0]][domainSize[1]][domainSize[2]];
		} catch (OutOfMemoryError e) {
			throw new MatrixTooBigException();
		}

		super.init(initialState);

		for (boolean [][] matrix2: matrix) {
			for (boolean[] matrix1: matrix2) {
				Arrays.fill(matrix1, initialState);		
			}
		}
		
		
	}

	@Override
	public boolean set(final int[] tuple, final boolean status) {
		final boolean[][][] matrix = this.matrix;

		if (matrix[tuple[0]][tuple[1]][tuple[2]] == status) {
			return false;
		}
		matrix[tuple[0]][tuple[1]][tuple[2]] ^= true;
		return true;
	}

	public boolean isTrue(final int[] tuple) {
		return super.isTrue(tuple) || matrix[tuple[0]][tuple[1]][tuple[2]];
	}

	public void intersect(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples) throws MatrixTooBigException {

		final boolean[][][] matrix;
		if (isActive()) {
			matrix = this.matrix.clone();
		} else {
			matrix = null;
		}

		generate(scope, constraintScope, supports, tuples, arity);

		if (matrix != null) {
			for (int i = matrix.length ; --i>=0;) {
				for (int j = matrix[i].length ; --j>=0;) {
					for (int k = matrix[i][j].length; --k >= 0;) {
						this.matrix[i][j][k] &= matrix[i][j][k];	
					}
				}
			}
		}
	}
}
