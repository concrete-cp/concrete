package cspfj.constraint;

import cspfj.problem.Variable;

public final class MatrixManager2D extends AbstractMatrixManager {

	private Matrix2D matrix;

	private int[] mask;

	// private int[] domain;

	private int currentPart;

	private int currentPosition;

	private int current;

	public MatrixManager2D(Variable[] scope, Matrix2D matrix) {
		super(scope, matrix);

		this.matrix = matrix;
	}

	// public void intersect(final Variable[] scope, final boolean supports,
	// final int[][] tuples) throws MatrixTooBigException {
	//
	// final Matrix2D matrix2D = this.matrix.clone();
	//
	// generate(scope, supports, tuples, 2);
	//
	// // if (matrix2D != null) {
	// // for (int i = 2; --i >= 0;) {
	// // for (int j = matrix2D[1 - i].length; --j >= 0;) {
	// // for (int k = matrix2D[1 - i][j].length; --k >= 0;) {
	// // this.matrix2D[1 - i][j][k] &= matrix2D[1 - i][j][k];
	// // }
	// // }
	// // }
	// // }
	//
	// }

	public boolean hasSupport(final int variablePosition, final int index) {
		final int[] mask = matrix.getBooleanArray(variablePosition, index);
		final int[] domain = variables[1 - variablePosition].getBooleanDomain();
		for (int part = 0; part < mask.length; part++) {
			checks++;
			if ((mask[part] & domain[part]) != 0) {
				tuple[variablePosition] = index;
				this.variablePosition = variablePosition;
				currentPart = part;
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean setFirstTuple(final int variablePosition, final int index) {
		this.variablePosition = variablePosition;
		tuple[variablePosition] = index;

		mask = matrix.getBooleanArray(variablePosition, index);
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
		final int[] domain = variables[1 - variablePosition].getBooleanDomain();

		while (current == 0) {
			currentPart++;

			if (currentPart >= mask.length) {
				return false;
			}
			// System.err.print("-");
			checks++;
			current = mask[currentPart] & domain[currentPart];
			// System.err.println(Integer.toBinaryString(current));
			currentPosition = -1;
		}

		// currentPosition++;
		// System.err.print(Integer.toBinaryString(current)) ;

		// final int mask0 = BooleanArray.MASKS[0];

		final int rotate = Integer.numberOfLeadingZeros(current) + 1;

		currentPosition += rotate;
		current <<= rotate;

		// current <<= 1;

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

		matrix.matrix = this.matrix.clone();

		matrix.mask = mask.clone();

		return matrix;
	}

	public String toString() {
		return "MatrixManager2D-" + matrix;
	}

	protected void updateResidues() {
		last[variablePosition][tuple[variablePosition]][0] = currentPart;
	}

	protected boolean controlResidue(final int position, final int index) {
		final int part = last[position][index][0];
		presenceChecks++;
		return part != -1
				&& (matrix.getBooleanArray(position, index)[part] & variables[1 - position]
						.getBooleanDomain()[part]) != 0;
	}

	protected Matrix unshareMatrix() {
		return matrix = (Matrix2D) super.unshareMatrix();
	}
}
