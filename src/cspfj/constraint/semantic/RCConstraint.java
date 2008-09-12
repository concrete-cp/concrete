package cspfj.constraint.semantic;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
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

	private final Map<Integer, SortedSet<Integer>> pending;

	public RCConstraint(Variable[] scope) {
		super(scope);

		intervals = initIntervals(scope);
		sortedIntervals = intervals.clone();

		pending = new HashMap<Integer, SortedSet<Integer>>(
				scope[0].getDomain().length);

	}

	public RCConstraint(Variable[] scope, String name) {
		super(scope, name);

		intervals = initIntervals(scope);
		sortedIntervals = intervals.clone();

		pending = new HashMap<Integer, SortedSet<Integer>>(
				scope[0].getDomain().length);

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
	public boolean removeTuple(int[] tuple) {
		final int removed = intervals[tuple[0]].remove(tuple[1]);

		if (removed > 0) {
			Arrays.sort(sortedIntervals);
			System.out.println(Arrays.toString(sortedIntervals));
			return true;
		}
		return false;

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

		boolean v0revised = false;

		int yLB = -1;
		int yUB = -1;

		int itvPointer = 0;
		boolean[] validated = new boolean[v0.getDomain().length];

		for (int i = v1.getFirst(); i >= 0; i = v1.getNext(i)) {
			while (itvPointer < sortedIntervals.length
					&& sortedIntervals[itvPointer].lb < i) {
				itvPointer++;
			}
			
		}

		for (int i0 = v0.getFirst(); i0 >= 0; i0 = v0.getNext(i0)) {
			boolean found = false;
			final Interval itv = intervals[i0];

			for (int i1 = itv.lb; i1 <= itv.ub; i1++) {
				if (v1.isPresent(i1)) {
					found = true;
					break;
				}
			}
			if (found) {
				if (yLB > itv.lb) {
					yLB = itv.lb;
				}
				if (yUB < 0 || yUB < itv.ub) {
					yUB = itv.ub;
				}
			} else {
				v0.remove(i0, level);
			}
		}

		if (v0revised) {
			if (v0.getDomainSize() < 1) {
				return false;
			}
			revisator.revised(this, v0);
		}

		boolean v1revised = false;

		for (int i1 = v1.getFirst(); i1 >= 0 && i1 < yLB; i1 = v1.getNext(i1)) {
			v1.remove(i1, level);
			v1revised = true;
		}
		for (int i1 = v1.getLast(); i1 >= 0 && i1 > yUB; i1 = v1.getPrev(i1)) {
			v1.remove(i1, level);
			v1revised = true;
		}

		if (v1revised) {
			if (v1.getDomainSize() < 1) {
				return false;
			}
			revisator.revised(this, v1);
		}

		return true;
	}

	private static class Interval implements Comparable<Interval> {
		private int lb;
		private int ub;
		private final int index0;
		private SortedSet<Integer> pending;

		public Interval(int lb, int ub, int index0) {
			this.lb = lb;
			this.ub = ub;
			this.index0 = index0;
		}

		public int incLb() {
			int removed = 0;
			lb++;
			removed++;

			if (pending != null) {
				while (!pending.isEmpty() && pending.first() < lb) {
					pending.remove(pending.first());
				}
				while (!pending.isEmpty() && pending.first() == lb) {
					lb++;
					removed++;
					pending.remove(pending.first());
				}
			}
			return removed;
		}

		public int decUb() {
			int removed = 0;
			ub--;
			removed++;

			if (pending != null) {
				while (!pending.isEmpty() && pending.last() > ub) {
					pending.remove(pending.last());
				}
				while (!pending.isEmpty() && pending.last() == ub) {
					ub--;
					removed++;
					pending.remove(pending.last());
				}
			}
			return removed;
		}

		public int remove(int value) {
			int removed = 0;
			if (value == lb) {
				removed += incLb();
			} else if (value == ub) {
				removed += decUb();
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

		@Override
		public int compareTo(Interval arg0) {
			return lb - arg0.lb;
		}

		public String toString() {
			return index0 + ": [" + lb + ", " + ub + "]";
		}

	}
}
