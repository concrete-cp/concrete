package cspfj.constraint;

import cspfj.constraint.extension.TupleManager;
import cspfj.problem.IntVariable;

public abstract class AbstractAC3Constraint extends AbstractPVRConstraint {

	protected int[][][] last;

	private final int[] startTuple;

	protected TupleManager tupleManager;

	private final IntVariable[] intScope;

	// private static final Logger logger = Logger
	// .getLogger(AbstractAC3Constraint.class.getSimpleName());

	public AbstractAC3Constraint(IntVariable... scope) {
		this(null, scope);
	}

	public AbstractAC3Constraint(String name, IntVariable... scope) {
		super(scope);
		this.intScope = scope;
		startTuple = new int[getArity()];

		int maxDomain = intScope[getArity() - 1].getDomain().maxSize();
		for (int i = getArity() - 1; --i >= 0;) {
			maxDomain = Math.max(maxDomain, intScope[i].getDomain().maxSize());
		}

		last = new int[getArity()][maxDomain][];

		tupleManager = new TupleManager(this, tuple);

	}

	public boolean revise(final int position) {
		final IntVariable variable = intScope[position];

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

				variable.remove(index);

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

	public IntVariable getVariable(int position) {
		return intScope[position];
	}
	
	public IntVariable[] getScope() {
		return intScope;
	}

	// @Override
	// public void restore(int level) {
	// for (int p = getArity(); --p >= 0;) {
	// final Variable var = getVariable(p);
	// for (int i = var.getFirst(); i >= 0; i = var.getNext(i)) {
	// final Watches watches = this.watches[p][i];
	// if (watches.isToCheck()) {
	// int[] validResidue = null;
	//
	// for (int[] residue : watches) {
	// if (controlTuplePresence(residue, p)) {
	// validResidue = residue;
	// break;
	// }
	// }
	//
	// watches.clear();
	// if (validResidue != null) {
	// for (int p2 = getArity(); --p2 >= 0;) {
	// this.watches[p2][validResidue[p2]]
	// .add(validResidue);
	// }
	// }
	// watches.setToCheck(false);
	// }
	// }
	// }
	// }
}
