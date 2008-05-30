package cspfj.constraint;

import java.util.Arrays;
import java.util.Iterator;

public class TupleListDynamic implements Matrix, Cloneable, Iterable<int[]> {
	private int first;
	// private int last;

	private int[][] tuples;
	private int[] next;
	// private int[] prev;

	private int[] removed;
	private int[] removedLast;

	private int size;

	public TupleListDynamic(final int arity, final int nbTuples) {
		size = 0;
		first = -1;
		tuples = new int[nbTuples][arity];
		next = new int[nbTuples];
		Arrays.fill(next, -1);
		// prev = next.clone();
		removed = new int[arity];
		Arrays.fill(removed, -1);
		removedLast = removed.clone();

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

	@Override
	public boolean check(final int[] tuple) {
		if (contains(tuple)) {
			return true;
		}

		return false;
	}

	public void restore(final int level) {
		// for (int i = removed.length; --i >= level;) {
		if (level < removed.length && removed[level] >= 0) {
			addAll(removed[level], removedLast[level]);
			removedLast[level] = removed[level] = -1;
		}
		// }

	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status) {
			add(tuple.clone());
		} else {
			remove(tuple);
		}
	}

	public void add(final int[] tuple) {
		final int index = size++;
		System.arraycopy(tuple, 0, tuples[index], 0, tuple.length);
		addCell(index);
	}

	public void addAll(final int index, final int last) {

		next[last] = first;
		first = index;

	}

	// public int last(final int index) {
	// if (next[index] < 0) {
	// return index;
	// }
	// return last(next[index]);
	// }

	public void addCell(final int index) {
		next[index] = first;
		first = index;
	}

	public boolean contains(final int[] tuple) {
		for (int i = size; --i >= 0;) {
			if (Arrays.equals(tuple, tuples[i])) {
				return true;
			}
		}
		return false;

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

	public TupleListDynamic clone() throws CloneNotSupportedException {
		final TupleListDynamic list = (TupleListDynamic) super.clone();

		list.tuples = new int[tuples.length][];
		for (int i = tuples.length; --i >= 0;) {
			list.tuples[i] = tuples[i].clone();
		}

		list.next = next.clone();
		// list.prev = prev.clone();
		list.removed = removed.clone();
		list.removedLast = removedLast.clone();

		return list;
	}

	@Override
	public LLIterator iterator() {
		return new LLIterator();
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();

		for (int[] tuple : this) {
			stb.append(Arrays.toString(tuple)).append(",");
		}
		return stb.toString();
	}

	public int count() {
		int count = 0;
		final LLIterator itr = iterator();
		while (itr.hasNext()) {
			count++;
			itr.next();
		}
		return count;
	}

	public class LLIterator implements Iterator<int[]> {

		private int current;

		private int prev;

		public LLIterator() {
			current = -1;
			prev = -1;
			// assert first != null;
			// assert last != null;
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
			return tuples[current];
		}

		@Override
		public void remove() {
			remove(0);
		}

		public void remove(final int level) {
			// System.out.print(call++ + " : ");
			// final int count = count();

			// final int toDelete;
			// if (current == -1) {
			// toDelete = last;
			// } else {
			// toDelete = prev;
			// }

			// System.out.println(toDelete);
			// if (prev[toDelete] < 0) {
			// first = next[toDelete];
			// } else {
			// next[prev[toDelete]] = next[toDelete];
			// }
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

	//
	// private static class Cell {
	// private final int[] tuple;
	// private Cell next;
	// private Cell prev;
	//
	// public Cell(final int[] tuple) {
	// this.tuple = tuple;
	// next = prev = null;
	// }
	//
	// public void add(final Cell cell) {
	// next = cell;
	// cell.prev = this;
	// cell.next = null;
	// }
	//
	// public void remove() {
	// if (prev != null) {
	// prev.next = next;
	// }
	// if (next != null) {
	// next.prev = prev;
	// }
	// }
	//
	// public boolean contains(final int[] tuple) {
	// if (Arrays.equals(this.tuple, tuple)) {
	// return true;
	// }
	// if (next == null) {
	// return false;
	// }
	// return next.contains(tuple);
	// }
	//
	// public String toString() {
	// if (next == null) {
	// return Arrays.toString(tuple);
	// }
	// return Arrays.toString(tuple) + "," + next.toString();
	// }
	//
	// }

	// public static void main(String[] args) {
	// final TupleListDynamic tuples = new TupleListDynamic(3, 10);
	//
	// tuples.add(new int[] { 0, 4, 1 });
	// tuples.add(new int[] { 0, 1, 1 });
	// tuples.add(new int[] { 0, 0, 2 });
	// tuples.add(new int[] { 0, 3, 1 });
	// tuples.add(new int[] { 1, 0, 1 });
	// tuples.add(new int[] { 0, 0, 4 });
	// tuples.add(new int[] { 0, 2, 1 });
	//
	// final LLIterator itr1 = tuples.iterator();
	// while (itr1.hasNext()) {
	// final int[] tuple = itr1.next();
	// if (tuple[1] != 3)
	// itr1.remove(0);
	// }
	//
	// // tuples.addAll(t2);
	// //
	// final LLIterator itr = tuples.iterator();
	// while (itr.hasNext()) {
	// final int[] tuple = itr.next();
	// System.out.print(Arrays.toString(tuple));
	// }
	//
	// System.out.println();
	//
	// tuples.restore(0);
	// final LLIterator itr2 = tuples.iterator();
	// while (itr2.hasNext()) {
	// final int[] tuple = itr2.next();
	// System.out.print(Arrays.toString(tuple));
	// }
	// System.out.println();
	//
	// //
	// System.out.println(Arrays.toString(tuples.tuples[tuples.first]));
	// }

}
