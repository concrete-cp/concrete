package cspfj.constraint;

import java.util.Arrays;
import java.util.Iterator;

import cspfj.problem.Variable;

public class MatrixManagerDynamic extends MatrixManager implements
		Iterable<int[]> {

	private final TupleArray tld;

	private int first;

	private int[] next;

	private int[] removed;
	private int[] removedLast;

	public MatrixManagerDynamic(final Variable[] scope, final TupleArray matrix) {
		super(scope, matrix);
		this.tld = matrix;
		first = -1;
		next = new int[tld.getNbTuples()];
		Arrays.fill(next, -1);
		// prev = next.clone();
		removed = new int[arity];
		Arrays.fill(removed, -1);
		removedLast = removed.clone();

		for (int i = tld.getNbTuples(); --i >= 0;) {
			if (tld.getTuple(i) != null && tld.getTuple(i).length == scope.length) {
				addCell(i);
			}
		}

	}

	private void expandRemoved(final int newLength) {

		final int[] newRemoved = new int[newLength];
		Arrays.fill(newRemoved, -1);
		System.arraycopy(removed, 0, newRemoved, 0, removed.length);

		final int[] newRemovedLast = newRemoved.clone();

		System.arraycopy(removedLast, 0, newRemovedLast, 0, removedLast.length);

		removed = newRemoved;
		removedLast = newRemovedLast;
	}

	public void restore(final int level) {
		// for (int i = removed.length; --i >= level;) {
		if (level < removed.length && removed[level] >= 0) {
			addAll(removed[level], removedLast[level]);
			removedLast[level] = removed[level] = -1;
		}
		// }

	}

	private void addAll(final int index, final int last) {

		next[last] = first;
		first = index;

	}

	private void addCell(final int index) {
		next[index] = first;
		first = index;
	}

	@Override
	public LLIterator iterator() {
		return new LLIterator();
	}

	public MatrixManagerDynamic clone() throws CloneNotSupportedException {
		final MatrixManagerDynamic list = (MatrixManagerDynamic) super.clone();
		list.next = next.clone();
		// list.prev = prev.clone();
		list.removed = removed.clone();
		list.removedLast = removedLast.clone();
		return list;

	}

	public void remove(final int[] tuple) {
		final Iterator<int[]> itr = this.iterator();
		while (itr.hasNext()) {
			if (Arrays.equals(itr.next(), tuple)) {
				itr.remove();
				break;
			}
		}
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();

		for (int[] tuple : this) {
			stb.append(Arrays.toString(tuple)).append(",");
		}
		return stb.toString();
	}

	public class LLIterator implements Iterator<int[]> {

		private int current;

		private int prev;

		public LLIterator() {
			current = -1;
			prev = -1;
		}

		@Override
		public boolean hasNext() {
			return (current < 0) ? (first != -1) : (next[current] != -1);
		}

		@Override
		public int[] next() {
			if (current < 0) {
				current = first;
			} else {
				prev = current;
				current = next[current];

			}
			return tld.getTuple(current);
		}

		@Override
		public void remove() {
			remove(0);
		}

		public void remove(final int level) {
			if (prev < 0) {
				first = next[current];
			} else {
				next[prev] = next[current];
			}

			// assert count() == count - 1 : count + "->" + count();

			if (level >= removed.length) {
				expandRemoved(level + 1);
			}

			final int oldFirstRemoved = removed[level];

			next[current] = oldFirstRemoved;
			removed[level] = current;

			if (oldFirstRemoved < 0) {
				removedLast[level] = current;
			}
			current = prev;

		}

	}

	public static void main(String[] args) {
		final TupleArray ta = new TupleArray();
		ta.set(new int[] { 1, 1, 1 }, true);
		ta.set(new int[] { 2, 2, 2 }, true);
		ta.set(new int[] { 3, 3, 3 }, true);

		final int[] dom = new int[] { 1, 2, 3 };

		final Variable[] scope = { new Variable(dom), new Variable(dom),
				new Variable(dom) };

		final MatrixManagerDynamic mm = new MatrixManagerDynamic(scope, ta);

		for (int[] t:mm) {
			System.out.print(Arrays.toString(t));
		}
		System.out.println();
		
		ta.set(new int[] {2,2,2}, false);
		
		for (int[] t:mm) {
			System.out.print(Arrays.toString(t));
		}
		System.out.println();
		
		
	}
}
