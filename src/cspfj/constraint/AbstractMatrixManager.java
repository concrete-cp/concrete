package cspfj.constraint;

import java.util.Arrays;

import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public abstract class AbstractMatrixManager implements Cloneable {
	protected final int[] domainSize;

	protected int[] tuple;

	protected Variable[] variables;

	protected final int arity;

	protected int variablePosition;

	protected int[][][] last;

	public AbstractMatrixManager(Variable[] scope, int[] tuple, int[][][] last) {
		super();

		variables = scope;
		domainSize = new int[scope.length];

		for (int i = scope.length; --i >= 0;) {
			domainSize[i] = scope[i].getDomain().length;
		}
		this.tuple = tuple;

		this.arity = scope.length;

		this.last = last;

	}

	public static AbstractMatrixManager factory(final Variable[] scope,
			final int[] tuple, final int[][][] last) {
		switch (scope.length) {
		case 2:
			return new MatrixManager2D(scope, tuple, last);

		default:
			return new MatrixManagerGeneral(scope, tuple, last);
		}
	}

	public abstract void init(final boolean supports)
			throws MatrixTooBigException;

	public abstract boolean set(final int[] tuple, final boolean status);

	public boolean removeTuple() {
		if (set(tuple, false)) {
			return true;
		}
		return false;
	}

	public boolean hasSupport(final int position, final int index) {
		return setFirstTuple(position, index);
	}

	public abstract boolean isTrue(final int[] tuple);

	public abstract void intersect(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples) throws MatrixTooBigException;

	protected void generate(final Variable[] scope,
			final Variable[] constraintScope, final boolean supports,
			final int[][] tuples, final int arity)
			throws MatrixTooBigException {
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

	public boolean setFirstTuple(final int variablePosition, final int index) {
		this.variablePosition = variablePosition;
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

	public AbstractMatrixManager deepCopy(Variable[] variables, int[] tuple)
			throws CloneNotSupportedException {
		final AbstractMatrixManager matrix = this.clone();
		matrix.tuple = tuple;
		matrix.variables = variables;
		return matrix;
	}

	public AbstractMatrixManager clone() throws CloneNotSupportedException {
		return (AbstractMatrixManager) super.clone();
	}

	public boolean check() {
		return isTrue(tuple);
	}

	protected boolean controlResidue(final int position, final int index) {
		if (last[position][index][0] != -1
				&& controlTuplePresence(last[position][index], position)) {
			return true;
		}
		return false;
	}

	protected void updateResidues() {
		for (int position = arity; --position >= 0;) {
			final int value = tuple[position];
			System.arraycopy(tuple, 0, last[position][value], 0, arity);
		}
	}

	protected boolean controlTuplePresence(final int[] tuple, final int position) {
		Constraint.nbPresenceChecks++;
		final Variable[] involvedVariables = this.variables;
		for (int i = arity; --i >= 0;) {
			if (i != position && !involvedVariables[i].isPresent(tuple[i])) {
				return false;
			}
		}

		return true;
	}
}
