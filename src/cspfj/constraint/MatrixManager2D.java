package cspfj.constraint;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;
import cspfj.util.BooleanArray;

public final class MatrixManager2D extends AbstractMatrixManager {

	private int[][][] matrix2D;

	private int[] mask;

	private int[] domain;

	private int currentPart;

	private int currentPosition;

	private int current;

	public MatrixManager2D(Variable[] scope, int[] tuple) {
		super(scope, tuple);
	}

	public void init(final boolean initialState) throws MatrixTooBigException {
		final int[] domainSize = this.domainSize;

		final int maxDomainSize = Math.max(domainSize[0], domainSize[1]);

		final int[][][] matrix = new int[2][maxDomainSize][];

		for (int i = 2; --i >= 0;) {
			for (int j = maxDomainSize; --j >= 0;) {

				matrix[i][j] = new int[BooleanArray
						.booleanArraySize(domainSize[1 - i])];

				BooleanArray.initBooleanArray(matrix[i][j], domainSize[1 - i],
						initialState);

			}
		}

		matrix2D = matrix;

	}

	@Override
	public boolean set(final int[] tuple, final boolean status) {
		BooleanArray.set(matrix2D[0][tuple[0]], tuple[1], status);
		return BooleanArray.set(matrix2D[1][tuple[1]], tuple[0], status);
	}

	@Override
	public boolean isTrue(final int[] tuple) {
		return BooleanArray.isTrue(matrix2D[0][tuple[0]], tuple[1]);
	}

	public void intersect(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples) throws MatrixTooBigException {

		final int[][][] matrix2D = this.matrix2D.clone();

		generate(scope, constraintScope, supports, tuples, 2);

		if (matrix2D != null) {
			for (int i = 2; --i >= 0;) {
				for (int j = matrix2D[1 - i].length; --j >= 0;) {
					for (int k = matrix2D[1 - i][j].length; --k >= 0;) {
						this.matrix2D[1 - i][j][k] &= matrix2D[1 - i][j][k];
					}
				}
			}
		}

	}
	
	public boolean hasSupport(final int variablePosition, final int index) {
		final int[] mask = matrix2D[variablePosition][index] ;
		for (int part = 0 ; part < mask.length ; part++) {
			if ((mask[part] & domain[part]) != 0) {
				return true ;
			}
		}
		return false ;
	}

	@Override
	public boolean setFirstTuple(final int variablePosition, final int index) {
		this.variablePosition = variablePosition;
		tuple[variablePosition] = index;

		mask = matrix2D[variablePosition][index];
		domain = variables[1 - variablePosition].getBooleanDomain();
		currentPart = -1;
		current = 0;
		return next();
	}

	@Override
	public boolean next() {
		int currentPosition = this.currentPosition;
		int current = this.current;
		int currentPart = this.currentPart;

		final int[] mask = this.mask;
		final int[] domain = this.domain;

		while (current == 0) {
			currentPart++;

			if (currentPart >= mask.length) {
				return false;
			}
			// System.err.print("-");
			current = mask[currentPart] & domain[currentPart];
			// System.err.println(Integer.toBinaryString(current));
			currentPosition = -1;
		}

		currentPosition++;
		// System.err.print(Integer.toBinaryString(current)) ;

		final int mask0 = BooleanArray.MASKS[0];

		while ((current & mask0) == 0) {
			currentPosition++;
			current <<= 1;
		}

		current <<= 1;

		this.currentPart = currentPart;
		this.current = current;
		this.currentPosition = currentPosition;

		tuple[1 - variablePosition] = currentPart * Integer.SIZE
				+ currentPosition;
		// System.err.println(" - " + tuple[1 - variablePosition]) ;
		// System.err.println(Integer.toBinaryString(current) + " " +
		// currentPart + " " + currentPosition);
		return true;
	}

	public MatrixManager2D clone() throws CloneNotSupportedException {
		final MatrixManager2D matrix = (MatrixManager2D) super.clone();

		final int maxDomainSize = Math.max(domainSize[0], domainSize[1]);

		if (matrix2D != null) {
			matrix.matrix2D = new int[2][maxDomainSize][];

			for (int i = 2; --i >= 0;) {
				for (int j = maxDomainSize; --j >= 0;) {
					matrix.matrix2D[i][j] = matrix2D[i][j].clone();
				}
			}
		}

		return matrix;
	}

	public String toString() {
		return "MatrixManager2D-" + matrix2D;
	}

}
