package cspfj.constraint.extension;

import java.util.Arrays;
import java.util.Iterator;

import cspfj.problem.Variable;

public class MatrixManagerDynamic extends MatrixManager implements
		Iterable<int[]> {

	private TupleSet tupleSet;

	private int[][] tupleList;

	private int first;

	private int[] next;

	private int[] removed;
	private int[] removedLast;

	public MatrixManagerDynamic(final Variable[] scope, final TupleSet matrix,
			final boolean shared) {
		super(scope, matrix, shared);
		this.tupleSet = matrix;
		tupleList = new int[matrix.size()][];

		int i = 0;
		for (Iterator<int[]> itr = matrix.iterator(); itr.hasNext(); i++) {
			tupleList[i] = itr.next();
		}
		first = -1;
		next = new int[tupleSet.size()];
		Arrays.fill(next, -1);

		removed = new int[arity];
		Arrays.fill(removed, -1);
		removedLast = removed.clone();

		for (i = tupleList.length; --i >= 0;) {
			if (tupleList[i] != null && tupleList[i].length == scope.length) {
				addCell(i);
			}
		}

	}

	private void expandRemoved(final int newLength) {

		final int[] newRemoved = Arrays.copyOf(removed, newLength);
		// Arrays.fill(newRemoved, removed.length, newRemoved.length, -1);

		final int[] newRemovedLast = Arrays.copyOf(removedLast, newLength);
		// Arrays.fill(newRemovedLast, removedLast.length,
		// newRemovedLast.length,
		// -1);

		removed = newRemoved;
		removedLast = newRemovedLast;
	}

	public void restore(final int level) {
		for (int i = removed.length; --i >= level;) {
			if (removed[i] >= 0) {
				addAll(removed[i], removedLast[i]);
				removedLast[i] = removed[i] = -1;
			}
		}

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

		private int current = first;

		private int prev = first;

		@Override
		public boolean hasNext() {
			return current >= 0;
		}

		@Override
		public int[] next() {
			prev = current;
			current = next[current];
			return tupleList[prev];
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

			if (removed[level] < 0) {
				removedLast[level] = current;
			}

			next[current] = removed[level];
			removed[level] = current;

			current = prev;

		}

	}

	protected Matrix unshareMatrix() {
		tupleSet = (TupleSet) super.unshareMatrix();
		return tupleSet;
	}

	// public static void main(String[] args) throws FailedGenerationException {
	// final TupleHashSet ta = new TupleHashSet(false);
	// ta.set(new int[] { 0, 0, 0 }, true);
	// ta.set(new int[] { 1, 1, 1 }, true);
	// ta.set(new int[] { 2, 2, 2 }, true);
	//
	// final int[] dom = new int[] { 0, 1, 2 };
	//
	// final Variable[] scope = { new Variable(dom), new Variable(dom),
	// new Variable(dom) };
	//
	// final ExtensionConstraintDynamic constraint = new
	// ExtensionConstraintDynamic(
	// scope, ta);
	//
	// constraint.revise(0);
	//
	// for (Variable v : scope) {
	// System.out.println(v.getCurrentDomain());
	// }
	//
	// System.out.println();
	// System.arraycopy(new int[] { 1, 1, 1 }, 0, constraint.getTuple(), 0, 3);
	//
	// constraint.removeTuple();
	//
	// constraint.revise(0);
	//
	// for (Variable v : scope) {
	// System.out.println(v.getCurrentDomain());
	// }
	// }
}
