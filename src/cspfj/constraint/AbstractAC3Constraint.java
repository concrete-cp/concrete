package cspfj.constraint;

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import cspfj.AbstractSolver;
import cspfj.problem.Variable;

public abstract class AbstractAC3Constraint extends AbstractPVRConstraint {

	protected int[][][] last;

	private final int[] startTuple;

	private Collection<int[]>[][] watches;

	private Collection<Integer>[] propagate;

	public static boolean useValueRemovalEvents;

	public AbstractAC3Constraint(Variable[] scope) {
		this(scope, null);
		useValueRemovalEvents = AbstractSolver.parameters
				.containsKey("ac.events");
	}

	public AbstractAC3Constraint(Variable[] scope, String name) {
		super(scope);
		useValueRemovalEvents = AbstractSolver.parameters
				.containsKey("ac.events");
		startTuple = new int[getArity()];

		int maxDomain = getVariable(getArity() - 1).getDomain().length;
		for (int i = getArity() - 1; --i >= 0;) {
			if (getVariable(i).getDomain().length > maxDomain) {
				maxDomain = getVariable(i).getDomain().length;
			}
		}

		last = new int[getArity()][maxDomain][];
		if (useValueRemovalEvents) {
			watches = new Watches[getArity()][maxDomain];
			propagate = new FixedIntList[getArity()];
		}
	}

	public boolean revise(final int position, final int level) {
		final Variable variable = getVariable(position);

		assert !variable.isAssigned();

		boolean revised = false;

		// logger.finer("Revising " + variable + " "
		// + Arrays.toString(variable.getCurrentDomain()) + " against "
		// + this);

		if (useValueRemovalEvents) {
			for (int index : propagate[position]) {

				// logger.finer("Checking (" + variable + ", " + index + ")");
				// if (conflictCounts && othersSize >
				// nbInitConflicts[position][index]) {
				// continue;
				// }

				if (variable.isPresent(index)
						&& !simpleFindValidTuple(position, index)) {
					// logger.finer("removing " + index + " from " + variable);

					variable.remove(index, level);

					revised = true;
					setActive(true);

				}

			}
			propagate[position].clear();
			return revised;
		}
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
		final int[] residue = tuple.clone();
		for (int position = getArity(); --position >= 0;) {
			if (useValueRemovalEvents) {
				if (watches[position][residue[position]] == null) {
					watches[position][residue[position]] = new Watches();
				} else {
					final int[] oldResidue = last[position][residue[position]];
					watches[position][residue[position]].remove(oldResidue);
				}
				watches[position][residue[position]].add(residue);
			}
			last[position][residue[position]] = residue;

		}
	}

	public void removeTupleCache() {
		last = null;
		watches = null;
		propagate = null;
	}

	@Override
	public boolean isSlow() {
		// TODO Auto-generated method stub
		return false;
	}

	public void deleted(int position, int index) {
		if (!useValueRemovalEvents || watches[position][index] == null) {
			return;
		}
		for (int[] residue : watches[position][index]) {
			for (int p = getArity(); --p >= 0;) {
				propagate[p].add(residue[p]);
			}
		}

	}

	private static class Watches extends LinkedList<int[]> {

		/**
		 * 
		 */
		private static final long serialVersionUID = 3669590222285240545L;

	}

	private static class FixedIntList extends AbstractCollection<Integer> {

		private final int[] contents;
		private int size;

		public FixedIntList(int size) {
			contents = new int[size];
			size = 0;
		}

		@Override
		public Iterator<Integer> iterator() {
			return new Iterator<Integer>() {
				private int position = 0;

				@Override
				public boolean hasNext() {
					return position < size;
				}

				@Override
				public Integer next() {
					return contents[position++];
				}

				@Override
				public void remove() {
					throw new UnsupportedOperationException();
				}

			};
		}

		@Override
		public int size() {
			return size;
		}

		@Override
		public void clear() {
			size = 0;
		}

	}
}
