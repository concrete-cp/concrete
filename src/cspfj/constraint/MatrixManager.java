package cspfj.constraint;

import java.util.Arrays;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public abstract class MatrixManager {
	protected final int[] domainSize;

	private boolean active = false;

	protected final int[] tuple;

	protected final Variable[] variables;

	public MatrixManager(Variable[] scope, int[] tuple) {
		super();

		variables = scope;
		domainSize = new int[scope.length];

		for (int i = scope.length; --i >= 0;) {
			domainSize[i] = scope[i].getDomain().length;
		}
		this.tuple = tuple;

	}

	public static MatrixManager factory(final Variable[] scope,
			final int[] tuple) {
		if (scope.length == 2) {
			return new MatrixManager2D(scope, tuple);
		}
		return new MatrixManagerGeneral(scope, tuple);

	}

	public void init(final boolean initialState) throws MatrixTooBigException {
		active = true;
	}

	public void clear() {
		active = false;
	}

	public abstract boolean set(final int[] tuple, final boolean status);

	public boolean removeTuple(final int[] tuple) {

		if (!active) {
			try {
				init(true);
			} catch (MatrixTooBigException e) {
				return false;
			}

		}

		if (set(tuple, false)) {
			return true;
		}
		return false;
	}

	public boolean isTrue(final int[] tuple) {
		return !active;
	}

	public abstract void intersect(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples) throws MatrixTooBigException;

	protected void generate(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples, final int arity) throws MatrixTooBigException {
		init(!supports);

		final int[] realTuple = this.tuple;
		if (Arrays.equals(scope, constraintScope)) {
			for (int[] tuple : tuples) {
				for (int i = arity; --i >= 0;) {
					realTuple[i] = scope[i].index(tuple[i]);
				}
				set(realTuple, supports);
			}
		} else {

			for (int[] tuple : tuples) {
				// final Variable[] involvedVariables = this.involvedVariables;

				for (int i = arity; --i >= 0;) {
					for (int j = arity; --j >= 0;) {
						if (scope[i] == constraintScope[j]) {
							realTuple[j] = scope[i].index(tuple[i]);
							break;
						}
					}
				}
				set(realTuple, supports);
			}
		}
	}

	public boolean isActive() {
		return active;
	}

	public abstract boolean setFirstTuple(final int variablePosition,
			final int index);

	public abstract boolean next();

}
