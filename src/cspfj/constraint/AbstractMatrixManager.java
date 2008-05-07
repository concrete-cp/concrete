package cspfj.constraint;

import cspfj.problem.Variable;

public abstract class AbstractMatrixManager implements Cloneable {
	protected final int[] domainSize;

	protected Variable[] variables;

	protected final int arity;

	protected int variablePosition;


	protected static long checks = 0;

	protected static long presenceChecks = 0;

	private Matrix matrix;

	private boolean shared = true;

	protected int[] tuple;

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
		if (shared) {
			unshareMatrix();
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

//	public boolean hasSupport(final int position, final int index) {
//		return setFirstTuple(position, index);
//	}

	public boolean isTrue(final int[] tuple) {
		return matrix.check(tuple);
	}

//	public boolean setFirstTuple(final int variablePosition, final int index) {
//		this.variablePosition = variablePosition;
//		for (int position = arity; --position >= 0;) {
//			if (position == variablePosition) {
//				tuple[position] = index;
//			} else {
//				tuple[position] = variables[position].getFirst();
//			}
//		}
//
//		if (!check()) {
//			return next();
//		}
//
//		return true;
//	}

//
//
//	private boolean setNextTuple() {
//		final int[] tuple = this.tuple;
//
//		final Variable[] involvedVariables = this.variables;
//		for (int i = arity; --i >= 0;) {
//			if (i == variablePosition) {
//				continue;
//			}
//
//			final int index = involvedVariables[i].getNext(tuple[i]);
//
//			if (index < 0) {
//				tuple[i] = involvedVariables[i].getFirst();
//			} else {
//				tuple[i] = index;
//				return true;
//			}
//		}
//		return false;
//
//	}

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
		checks++;
		return matrix.check(tuple);
	}

	public static long getChecks() {
		return checks;
	}

	public static long getPresenceChecks() {
		return presenceChecks;
	}

	private void firstT() {
		for (int p = variables.length; --p >= 0;) {
			tuple[p] = variables[p].getDomain().length - 1;
		}
	}

	private boolean nextT() {
		final int[] tuple = this.tuple;

		final Variable[] involvedVariables = this.variables;
		for (int i = arity; --i >= 0;) {
			tuple[i]--;

			if (tuple[i] < 0) {
				tuple[i] = involvedVariables[i].getDomain().length - 1;
			} else {
				return true;
			}
		}
		return false;
	}

	public void intersect(final Matrix matrix2) {
		firstT();
		do {
			if (!matrix2.check(tuple)) {
				set(tuple, false);
			}
		} while (nextT());

	}
	
	protected Matrix unshareMatrix() {
		try {
			matrix = matrix.clone();
			
		} catch (CloneNotSupportedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		shared = false;
		return matrix;
	}
	
	public String getType() {
		return this.getClass().getSimpleName() + " w/ " + matrix.getClass().getSimpleName();
	}
}
