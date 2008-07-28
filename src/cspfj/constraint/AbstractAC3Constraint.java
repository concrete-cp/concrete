package cspfj.constraint;

import java.util.Arrays;

import cspfj.problem.Variable;

public abstract class AbstractAC3Constraint extends AbstractPVRConstraint {

	protected int[][][] last;

	private final int[] startTuple;

	public AbstractAC3Constraint(Variable[] scope) {
		this(scope, null);
	}

	public AbstractAC3Constraint(Variable[] scope, String name) {
		super(scope);

		startTuple = new int[getArity()];

		int maxDomain = 0;
		for (int i = getArity(); --i >= 0;) {
			if (getVariable(i).getDomain().length > maxDomain) {
				maxDomain = getVariable(i).getDomain().length;
			}
		}

		last = new int[getArity()][maxDomain][getArity()];
		for (int i = getArity(); --i >= 0;) {
			for (int j = maxDomain; --j >= 0;) {
				Arrays.fill(last[i][j], -1);
			}
		}
	}




	public boolean revise(final int position, final int level) {
		final Variable variable = getVariable(position);

		assert !variable.isAssigned();

		boolean revised = false;

		// logger.finer("Revising " + variable + " "
		// + Arrays.toString(variable.getCurrentDomain()) + " against "
		// + this);

		for (int index = variable.getFirst(); index >= 0; index = variable
				.getNext(index)) {

			// logger.finer("Checking (" + variable + ", " + index + ")");
			// if (conflictCounts && othersSize >
			// nbInitConflicts[position][index]) {
			// continue;
			// }

			if (!findValidTuple(position, index)) {
				// logger.finer("removing " + index + " from " + variable);

				variable.remove(index, level);

				revised = true;
				setActive(true);

			}

		}

		return revised;
	}

	private boolean findValidTuple2(final int variablePosition, final int index) {
		assert this.isInvolved(getVariable(variablePosition));

		// final long size = size()
		// / involvedVariables[variablePosition].getDomainSize();

		final boolean residue = last[variablePosition][index][0] != -1;

		if (residue
				&& controlTuplePresence(last[variablePosition][index],
						variablePosition)) {
			return true;
		}

		// final List<int[]> checkedAfter = new ArrayList<int[]>();

		final boolean twoWays = residue
				&& tupleManager.setTupleAfter(last[variablePosition][index],
						variablePosition);
		if (twoWays) {
			System.arraycopy(tuple, 0, startTuple, 0, getArity());
		} else {
			tupleManager.setFirstTuple(variablePosition, index);
		}

		do {
			// checkedAfter.add(tuple.clone());

			if (chk()) {
				updateResidues();
				return true;
			}

		} while (tupleManager.setNextTuple(variablePosition));

		// final List<int[]> checkedBefore = new ArrayList<int[]>();

		if (twoWays) {
			tupleManager.setTuple(startTuple);
			while (tupleManager.setPrevTuple(variablePosition)) {
				// checkedBefore.add(tuple.clone());
				if (chk()) {
					updateResidues();
					return true;
				}
			}
		}

		// assert (checkedAfter.size() + checkedBefore.size()) == size :
		// "checked "
		// + viewDomains()
		// + " ("
		// + size
		// + ", fixed = "
		// + variablePosition
		// + ") : "
		// + Arrays.toString(last[variablePosition][index])
		// + " / "
		// + viewTuples(checkedAfter)
		// + " / "
		// + viewTuples(checkedBefore);

		return false;

	}

	// private String viewTuples(final List<int[]> tuples) {
	// final StringBuilder stb = new StringBuilder("{ ");
	// for (int[] tuple : tuples) {
	// stb.append(Arrays.toString(tuple)).append(", ");
	// }
	// return stb.append(" }(").append(tuples.size()).append(")").toString();
	// }
	//
	// private String viewDomains() {
	// final StringBuilder stb = new StringBuilder("{ ");
	// for (Variable v : involvedVariables) {
	// stb.append(v.getCurrentDomain()).append(", ");
	// }
	// return stb.append(" }").toString();
	// }

	public boolean findValidTuple(final int variablePosition, final int index) {
		return findValidTuple1(variablePosition, index);
	}

	private boolean findValidTuple1(final int variablePosition, final int index) {
		assert this.isInvolved(getVariable(variablePosition));
		assert index >= 0;
		final boolean residue = last[variablePosition][index][0] != -1;

		if (residue
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

	protected void updateResidues() {
		for (int position = getArity(); --position >= 0;) {
			System.arraycopy(tuple, 0, last[position][tuple[position]], 0,
					getArity());
		}

	}

	public void removeTupleCache() {
		last = null;
	}

	@Override
	public boolean isSlow() {
		// TODO Auto-generated method stub
		return false;
	}

}
