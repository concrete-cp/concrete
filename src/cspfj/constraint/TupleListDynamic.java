package cspfj.constraint;

import java.util.Arrays;
import java.util.Iterator;

public class TupleListDynamic implements Matrix, Cloneable, Iterable<int[]> {
	private int first;
	private int last;

	private final int[][] tuples;
	private final int[] next;
	private final int[] prev;

	private final int[] removed;
	private final int[] removedLast;

	private int size;

	public TupleListDynamic(final int arity, final int nbTuples) {
		size = 0;
		first = last = -1;
		tuples = new int[nbTuples][arity];
		next = new int[nbTuples];
		Arrays.fill(next, -1);
		prev = next.clone();

		removed = new int[arity];
		Arrays.fill(removed, -1);
		removedLast = removed.clone();
	}

	@Override
	public boolean check(final int[] tuple) {
		if (contains(tuple)) {
			return true;
		}

		return false;
	}

	public void restore(final int level) {
		for (int i = level; i < removed.length && removed[i] >= 0; i++) {
			addAll(removed[level], removedLast[level]);
			removedLast[level] = removed[level] = -1;
		}

	}

	public void save(final int[] tuple, final int level) {

	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == false) {
			remove(tuple);
		} else {
			add(tuple.clone());
		}
	}

	public void add(final int[] tuple) {
		final int index = size++;
		System.arraycopy(tuple, 0, tuples[index], 0, tuple.length);
		addCell(index);
	}

	public void addAll(final int index, final int last) {

		if (first == -1) {
			first = index;

		} else {
			next[last] = index;
			prev[index] = last;
		}
		this.last = last;
	}

	// public int last(final int index) {
	// if (next[index] < 0) {
	// return index;
	// }
	// return last(next[index]);
	// }

	public void addCell(final int index) {
		if (last == -1) {
			first = last = index;
			next[index] = -1;
			prev[index] = -1;
		} else {
			add(last, index);
			last = index;
		}

	}

	public void add(final int last, final int index) {
		next[last] = index;
		prev[index] = last;
		next[index] = -1;
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

		// for (int i = tuples.length; --i >= 0;) {
		// System.arraycopy(tuples[i], 0, list.tuples[i], 0, tuples[i].length);
		// }
		System.arraycopy(next, 0, list.next, 0, next.length);
		System.arraycopy(prev, 0, list.prev, 0, prev.length);
		System.arraycopy(removed, 0, list.removed, 0, removed.length);

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

	int call = 0;

	public class LLIterator implements Iterator<int[]> {

		private int current;

		public LLIterator() {
			current = first;
			// assert first != null;
			// assert last != null;
		}

		@Override
		public boolean hasNext() {

			return current >= 0;
		}

		@Override
		public int[] next() {
			final int[] tuple = tuples[current];

			current = next[current];

			return tuple;
		}

		@Override
		public void remove() {

		}

		public int count(int index) {
			if (index == -1) {
				return 0;
			}
			return 1 + count(next[index]);
		}

		public void remove(final int level) {
			System.out.print(call++ + " : ");
			final int count = count(first);

			final int toDelete;
			if (current == -1) {
				toDelete = last;
			} else {
				toDelete = prev[current];
			}
			
			assert next[toDelete]==current;

			System.out.println(toDelete);
			if (prev[toDelete] < 0) {
				first = next[toDelete];
			} else {
				next[prev[toDelete]] = next[toDelete];
			}
			if (next[toDelete] < 0) {
				last = prev[toDelete];
			} else {
				prev[next[toDelete]] = prev[toDelete];
			}

			assert count(first) == count - 1 : count + "->" + count(first);

			final int secondRemoved = removed[level];
			removed[level] = toDelete;
			next[toDelete] = secondRemoved;
			if (secondRemoved < 0) {
				removedLast[level] = toDelete;
			} else {
				prev[secondRemoved] = toDelete;
			}
			prev[toDelete] = -1;
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

	public static void main(String[] args) {
		final TupleListDynamic tuples = new TupleListDynamic(3, 10);

		tuples.add(new int[] { 0, 4, 1 });
		tuples.add(new int[] { 0, 1, 1 });
		tuples.add(new int[] { 0, 0, 2 });
		tuples.add(new int[] { 0, 3, 1 });
		tuples.add(new int[] { 1, 0, 1 });
		tuples.add(new int[] { 0, 0, 4 });
		tuples.add(new int[] { 0, 2, 1 });

		final LLIterator itr1 = tuples.iterator();
		while (itr1.hasNext()) {
			final int[] tuple = itr1.next();
			if (tuple[1] != 3)
				itr1.remove(0);
		}

		// tuples.addAll(t2);
		//	
		final LLIterator itr = tuples.iterator();
		while (itr.hasNext()) {
			final int[] tuple = itr.next();
			System.out.print(Arrays.toString(tuple));
		}

		System.out.println();

		tuples.restore(0);
		final LLIterator itr2 = tuples.iterator();
		while (itr2.hasNext()) {
			final int[] tuple = itr2.next();
			System.out.print(Arrays.toString(tuple));
		}
		System.out.println();

		//	
		System.out.println(Arrays.toString(tuples.tuples[tuples.first]));
		System.out.println(Arrays.toString(tuples.tuples[tuples.last]));
	}

}
