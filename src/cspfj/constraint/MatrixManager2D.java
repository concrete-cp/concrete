package cspfj.constraint;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;
import cspfj.util.BooleanArray;

public final class MatrixManager2D extends MatrixManager {

	private final int[][][] matrix2D;

	private int[] mask;

	private int[] domain;

	private int currentPart;

	private int currentPosition;

	private int current;

	public MatrixManager2D(Variable[] scope, int[] tuple) {
		super(scope, tuple);
		matrix2D = new int[2][][];
	}

	public void init(final boolean initialState) throws MatrixTooBigException {
		super.init(initialState);
		final int[] domainSize = this.domainSize;
		final int[][][] matrix2D = this.matrix2D;
		for (int i = 2; --i >= 0;) {
			matrix2D[i] = new int[domainSize[i]][BooleanArray
					.booleanArraySize(domainSize[1 - i])];
			for (int[] array : matrix2D[i]) {
				BooleanArray.initBooleanArray(array, domainSize[1 - i],
						initialState);
			}
		}

	}

	@Override
	public boolean set(final int[] tuple, final boolean status) {
		BooleanArray.set(matrix2D[0][tuple[0]], tuple[1], status);
		return BooleanArray.set(matrix2D[1][tuple[1]], tuple[0], status);
	}

	@Override
	public boolean isTrue(final int[] tuple) {
		return super.isTrue(tuple)
				|| BooleanArray.isTrue(matrix2D[0][tuple[0]], tuple[1]);
	}

	public void intersect(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples) throws MatrixTooBigException {

		final int[][][] matrix2D;
		if (isActive()) {
			matrix2D = this.matrix2D.clone();
		} else {
			matrix2D = null;
		}

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

	@Override
	public boolean setFirstTuple(final int variablePosition, final int index) {
		this.variablePosition = variablePosition;
		tuple[variablePosition] = index;

		if (isActive()) {
			mask = matrix2D[variablePosition][index];
			domain = variables[1 - variablePosition].getBooleanDomain();
			currentPart = -1;
			current = 0;
			return next();
		}

		current = tuple[1 - variablePosition] = variables[1 - variablePosition]
				.getFirst();
		return true;

	}

	@Override
	public boolean next() {
		if (!isActive()) {
			current = tuple[1 - variablePosition] = variables[1 - variablePosition]
					.getNext(current);
			if (current < 0) {
				return false;
			}
			return true;
		}

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

}
