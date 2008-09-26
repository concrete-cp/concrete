package cspfj.constraint.semantic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public class RCConstraint extends AbstractConstraint implements
		DynamicConstraint {

	private final Interval[] intervals;

	private final Interval[] sortedIntervals;

	private final BitSet v1Validated;

	public RCConstraint(Variable[] scope) {
		super(scope);

		intervals = initIntervals(scope);
		sortedIntervals = intervals.clone();

		v1Validated = new BitSet(scope[1].getDomain().length);
		init(scope);
	}

	public RCConstraint(Variable[] scope, String name) {
		super(scope, name);

		intervals = initIntervals(scope);
		sortedIntervals = intervals.clone();

		v1Validated = new BitSet(scope[1].getDomain().length);
		init(scope);
	}

	private void init(Variable[] scope) {
		nbInitConflicts = new long[getArity()][];
		nbMaxConflicts = new long[getArity()];
		for (int i = getArity(); --i >= 0;) {
			nbInitConflicts[i] = new long[scope[i].getDomain().length];
		}
	}

	private static Interval[] initIntervals(final Variable[] scope) {
		final Interval[] intervals = new Interval[scope[0].getDomain().length];
		final int ub = scope[1].getDomain().length - 1;
		for (int i = intervals.length; --i >= 0;) {
			intervals[i] = new Interval(0, ub, i);
		}
		return intervals;
	}

	@Override
	public int removeTuple(int[] tuple) {
		final Collection<Integer> removed = intervals[tuple[0]]
				.remove(tuple[1]);

		if (removed != null) {
			Arrays.sort(sortedIntervals);

			for (int i : removed) {
				tuple[1] = i;
				addConflict(tuple);
			}

			return removed.size();
		}
		return 0;

	}

	@Override
	public boolean check() {
		return intervals[tuple[0]].in(tuple[1]);
	}

	@Override
	public boolean isSlow() {
		return false;
	}

	@Override
	public boolean revise(int level, RevisionHandler revisator) {

		final Variable v0 = getVariable(0);
		final Variable v1 = getVariable(1);

		// Support condition
		if (v1.getDomainSize() > nbMaxConflicts[0]
				&& v0.getDomainSize() > nbMaxConflicts[1]) {
			return true;
		}

		v1Validated.clear();

		int itv = 0;

		boolean revised = false;

		for (int i = v1.getFirst(); i >= 0 && itv < sortedIntervals.length; i = v1
				.getNext(i)) {
			for (; itv < sortedIntervals.length && sortedIntervals[itv].lb <= i; itv++) {
				final Interval currentItv = sortedIntervals[itv];
				if (v0.isPresent(currentItv.index0)) {
					if (i <= currentItv.ub) {
						v1Validated.set(currentItv.lb, currentItv.ub + 1);
					} else {
						v0.remove(currentItv.index0, level);
						if (v0.getDomainSize() == 0) {
							return false;
						}
						revised = true;
					}
				}
			}
		}
		
		if (v1Validated.isEmpty()) {
			return false;
		}
		
		if (revised) {
			revisator.revised(this, v0);
		}

		revised = false;
		final int end = v1.getLast();
		for (int i = v1.getFirst(); i >= 0 && i <= end;) {
			if (v1Validated.get(i)) {
				i = v1Validated.nextClearBit(i);
			} else {
				if (v1.isPresent(i)) {
					v1.remove(i, level);
					if (v1.getDomainSize() == 0) {
						return false;
					}
					revised = true;
				}
				i = v1.getNextPresent(i);
			}
		}

		if (revised) {
			revisator.revised(this, v1);
		}

		return true;
	}

	// private static boolean intersect(Variable v, BitSet validated, int level)
	// {
	// boolean changed = false;
	//
	// int i = v.getFirst();
	//
	// while (i >= 0) {
	//
	// final int j = validated.nextClearBit(i);
	// if (i == j && v.isPresent(j)) {
	// v.remove(i, level);
	// changed = true;
	// } else if (j >= v.getDomain().length) {
	// break;
	// } else if (v.isPresent(j)) {
	// v.remove(j, level);
	// changed = true;
	// }
	// i = v.getNext(j);
	//
	// }
	// return changed;
	// }

	private static boolean intersect(Variable v, BitSet validated, int level) {
		boolean changed = false;
		final int end = v.getLast();
		for (int i = v.getFirst(); i >= 0 && i <= end;) {
			if (validated.get(i)) {
				i = validated.nextClearBit(i);
			} else {
				if (v.isPresent(i)) {
					v.remove(i, level);
					changed = true;
				}
				i = v.getNextPresent(i);
			}
		}
		return changed;
	}

	// private static boolean intersect(Variable v, BitSet validated, int level)
	// {
	// boolean changed = false;
	// for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
	// if (!validated.get(i)) {
	//
	// v.remove(i, level);
	// changed = true;
	//
	// }
	// }
	// return changed;
	// }

	// private static boolean intersect(Variable v, BitSet validated, int level)
	// {
	// boolean changed = false;
	// final int end = v.getLast();
	// for (int i = validated.nextClearBit(0); i <= end; i = validated
	// .nextClearBit(i + 1)) {
	// if (v.isPresent(i)) {
	// v.remove(i, level);
	// changed = true;
	// }
	// }
	// return changed;
	// }

	private static class Interval implements Comparable<Interval> {
		private int lb;
		private int ub;
		private final int index0;
		private SortedSet<Integer> pending;
		private static final Collection<Integer> removed = new ArrayList<Integer>();

		public Interval(int lb, int ub, int index0) {
			this.lb = lb;
			this.ub = ub;
			this.index0 = index0;
		}

		public void incLb() {
			removed.add(lb++);

			if (pending != null) {
				while (!pending.isEmpty() && pending.first() < lb) {
					pending.remove(pending.first());
				}
				while (!pending.isEmpty() && pending.first() == lb) {
					removed.add(lb++);
					pending.remove(pending.first());
				}
			}
		}

		public void decUb() {
			removed.add(ub--);

			if (pending != null) {
				while (!pending.isEmpty() && pending.last() > ub) {
					pending.remove(pending.last());
				}
				while (!pending.isEmpty() && pending.last() == ub) {
					removed.add(ub--);
					pending.remove(pending.last());
				}
			}
		}

		public Collection<Integer> remove(final int value) {
			removed.clear();
			if (value == lb) {

				incLb();

			} else if (value == ub) {

				decUb();

			} else if (lb < value && value < ub) {

				if (pending == null) {
					pending = new TreeSet<Integer>();
				}
				pending.add(value);

			}
			return removed;
		}

		public boolean in(final int value) {
			return lb <= value && value <= ub;
		}

		public String toString() {
			return index0 + ": [" + lb + ", " + ub + "]";
		}

		@Override
		public int compareTo(Interval arg0) {
			return lb - arg0.lb;
		}
	}

}
