package cspfj.constraint;

import cspfj.problem.Variable;

public final class MatrixManager2D extends MatrixManager {

	private Matrix2D matrix;

	private int[] mask;

	private final int[][] last;

	private static long checks = 0;

	private static long presenceChecks = 0;

	public MatrixManager2D(Variable[] scope, Matrix2D matrix,
			final boolean shared) {
		super(scope, matrix, shared);

		this.matrix = matrix;

		int length = Math.max(scope[0].getDomain().length,
				scope[1].getDomain().length);

		if (length > 3 * Integer.SIZE) {
			last = new int[2][Math.max(scope[0].getDomain().length, scope[1]
					.getDomain().length)];
		} else {
			last = null;
		}

	}

	public static long getChecks() {
		return checks;
	}

	public static long getPresenceChecks() {
		return presenceChecks;
	}

	public static final void clearStats() {
		checks = presenceChecks = 0;
	}

	public boolean hasSupport(final int variablePosition, final int index) {
		return last == null ? hasSupportNR(variablePosition, index)
				: hasSupportR(variablePosition, index);
	}

	private boolean hasSupportR(final int variablePosition, final int index) {
		if (controlResidue(variablePosition, index)) {
			return true;
		}

		final int[] mask = matrix.getBooleanArray(variablePosition, index);
		final int[] domain = variables[1 - variablePosition].getBooleanDomain();
		for (int part = mask.length; --part >= 0;) {
			checks++;
			if ((mask[part] & domain[part]) != 0) {
				last[variablePosition][index] = part;
				return true;
			}
		}
		return false;
	}
	
	private boolean hasSupportNR(final int variablePosition, final int index) {
		final int[] mask = matrix.getBooleanArray(variablePosition, index);
		final int[] domain = variables[1 - variablePosition].getBooleanDomain();
		for (int part = mask.length; --part >= 0;) {
			checks++;
			if ((mask[part] & domain[part]) != 0) {
				return true;
			}
		}
		return false;
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

	private boolean controlResidue(final int position, final int index) {
		final int part = last[position][index];
		presenceChecks++;
		return part != -1
				&& (matrix.getBooleanArray(position, index)[part] & variables[1 - position]
						.getBooleanDomain()[part]) != 0;
	}

	protected Matrix unshareMatrix() {
		return matrix = (Matrix2D) super.unshareMatrix();
	}
}
