package cspfj.constraint;

import cspfj.problem.Variable;

public abstract class AbstractAC3Constraint extends AbstractPVRConstraint {

	protected int[][][] last;

	private final int[] startTuple;

	public AbstractAC3Constraint(Variable... scope) {
		this(null, scope);
	}

	public AbstractAC3Constraint(String name, Variable... scope) {
		super(scope);
		startTuple = new int[getArity()];

		int maxDomain = getVariable(getArity() - 1).getDomain().maxSize();
		for (int i = getArity() - 1; --i >= 0;) {
			maxDomain = Math.max(maxDomain, getVariable(i).getDomain()
					.maxSize());
		}

		last = new int[getArity()][maxDomain][];
	}

	public boolean revise(final int position) {
		final Variable variable = getVariable(position);

		assert !variable.isAssigned();

		boolean revised = false;

		for (int index = variable.getFirst(); index >= 0; index = variable
				.getNext(index)) {

			if (!findValidTuple(position, index)) {
				variable.remove(index);

				revised = true;
				setActive(true);

			}

		}

		return revised;
	}

	private boolean findValidTuple2(final int variablePosition, final int index) {
		assert this.isInvolved(getVariable(variablePosition));

		final boolean residue = last[variablePosition][index][0] != -1;

		if (residue
				&& controlTuplePresence(last[variablePosition][index],
						variablePosition)) {
			return true;
		}

		final boolean twoWays = residue
				&& tupleManager.setTupleAfter(last[variablePosition][index],
						variablePosition);
		if (twoWays) {
			System.arraycopy(tuple, 0, startTuple, 0, getArity());
		} else {
			tupleManager.setFirstTuple(variablePosition, index);
		}

		do {
			if (chk()) {
				updateResidues();
				return true;
			}

		} while (tupleManager.setNextTuple(variablePosition));

		if (twoWays) {
			tupleManager.setTuple(startTuple);
			while (tupleManager.setPrevTuple(variablePosition)) {
				if (chk()) {
					updateResidues();
					return true;
				}
			}
		}
		return false;

	}

	public boolean findValidTuple(final int variablePosition, final int index) {
		return findValidTuple1(variablePosition, index);
	}

	private boolean findValidTuple1(final int variablePosition, final int index) {
		assert this.isInvolved(getVariable(variablePosition));
		assert index >= 0;

		if (last[variablePosition][index] != null
				&& controlTuplePresence(last[variablePosition][index],
						variablePosition)) {
			return true;
		}

		tupleManager.setFirstTuple(variablePosition, index);

		do {
			if (chk()) {
				updateResidues();
				return true;
			}
		} while (tupleManager.setNextTuple(variablePosition));

		return false;

	}

	private boolean simpleFindValidTuple(final int variablePosition,
			final int index) {
		tupleManager.setFirstTuple(variablePosition, index);

		do {
			if (chk()) {
				updateResidues();
				return true;
			}
		} while (tupleManager.setNextTuple(variablePosition));

		return false;
	}

	protected void updateResidues() {
		if (last != null) {
			final int[] residue = tuple.clone();
			for (int position = getArity(); --position >= 0;) {
				last[position][residue[position]] = residue;
			}
		}
	}

	public void removeTupleCache() {
		last = null;
	}
}
