package cspfj.constraint;

import cspfj.problem.Variable;

public abstract class AbstractMatrixManager implements Cloneable {
	protected final int[] domainSize;

	protected int[] tuple;

	protected Variable[] variables;

	protected final int arity;

	protected int variablePosition;

	protected int[][][] last;

	protected static long checks = 0;

	protected static long presenceChecks = 0;

	private Matrix matrix;

	public AbstractMatrixManager(Variable[] scope, Matrix matrix) {
		super();

		variables = scope;
		domainSize = new int[scope.length];

		for (int i = scope.length; --i >= 0;) {
			domainSize[i] = scope[i].getDomain().length;
		}
		this.arity = scope.length;

		this.matrix = matrix;
	}

	public void setTuple(final int[] tuple) {
		this.tuple = tuple;
	}

	public void setLast(final int[][][] last) {
		this.last = last;
	}

	public static AbstractMatrixManager factory(final Variable[] scope,
			final Matrix matrix) {
		if (matrix instanceof Matrix2D) {
			return new MatrixManager2D(scope, (Matrix2D) matrix);
		}
		return new MatrixManagerGeneral(scope, matrix);

	}


	public boolean set(final int[] tuple, final boolean status) {
		if (matrix.check(tuple) == status) {
			return false;
		}
		matrix.set(tuple, status);
		return true;
	}

	public boolean removeTuple() {
		if (set(tuple, false)) {
			return true;
		}
		return false;
	}

	public boolean hasSupport(final int position, final int index) {
		return setFirstTuple(position, index);
	}

	public boolean isTrue(final int[] tuple) {
		return matrix.check(tuple);
	}

//	public abstract void intersect(final Variable[] scope,
//			final boolean supports, final int[][] tuples)
//			throws MatrixTooBigException;

//	protected void generate(final Variable[] scope, final boolean supports,
//			final int[][] tuples, final int arity) throws MatrixTooBigException {
//		init(!supports);
//		//
//		final int[] realTuple = this.tuple;
//		// if (Arrays.equals(scope, constraintScope)) {
//		for (int[] tuple : tuples) {
//			for (int i = arity; --i >= 0;) {
//				realTuple[i] = scope[i].index(tuple[i]);
//			}
//			set(realTuple, supports);
//		}
//		// } else {
//		//
//		// for (int[] tuple : tuples) {
//		// // final Variable[] involvedVariables = this.involvedVariables;
//		//
//		// for (int i = arity; --i >= 0;) {
//		// for (int j = arity; --j >= 0;) {
//		// if (scope[i] == constraintScope[j]) {
//		// realTuple[j] = scope[i].index(tuple[i]);
//		// break;
//		// }
//		// }
//		// }
//		// set(realTuple, supports);
//		// }
//		// }
//	}

	public boolean setFirstTuple(final int variablePosition, final int index) {
		this.variablePosition = variablePosition;
		for (int position = arity; --position >= 0;) {
			if (position == variablePosition) {
				tuple[position] = index;
			} else {
				tuple[position] = variables[position].getFirst();
			}
		}

		if (!check()) {
			return next();
		}

		return true;
	}

	public boolean next() {
		do {
			if (!setNextTuple()) {
				return false;
			}
		} while (!isTrue(tuple));

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

	public AbstractMatrixManager deepCopy(final Variable[] variables,
			final int[] tuple) throws CloneNotSupportedException {
		final AbstractMatrixManager matrix = this.clone();
		matrix.tuple = tuple;
		matrix.variables = variables;
		return matrix;
	}

	public AbstractMatrixManager clone() throws CloneNotSupportedException {
		return (AbstractMatrixManager) super.clone();
	}

	public boolean check() {
		return matrix.check(tuple);
	}

	protected boolean controlResidue(final int position, final int index) {
		return last[position][index][0] != -1
				&& controlTuplePresence(last[position][index], position);
	}

	protected void updateResidues() {
		for (int position = arity; --position >= 0;) {
			final int value = tuple[position];
			System.arraycopy(tuple, 0, last[position][value], 0, arity);
		}
	}

	protected boolean controlTuplePresence(final int[] tuple, final int position) {
		presenceChecks++;
		final Variable[] involvedVariables = this.variables;
		for (int i = arity; --i >= 0;) {
			if (i != position && !involvedVariables[i].isPresent(tuple[i])) {
				return false;
			}
		}

		return true;
	}

	public static long getChecks() {
		return checks;
	}

	public static long getPresenceChecks() {
		return presenceChecks;
	}
}
