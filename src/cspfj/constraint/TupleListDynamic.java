package cspfj.constraint;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import cspfj.constraint.TupleListDynamic.LList.LLIterator;

public class TupleListDynamic implements Matrix, Cloneable, Iterable<int[]> {

	private final LList tuples;

	private Map<Integer, LList> removed;

	public TupleListDynamic() {
		removed = new HashMap<Integer, LList>();
		tuples = new LList();
	}

	@Override
	public boolean check(final int[] tuple) {
		if (tuples.contains(tuple)) {
			return true;
		}
		for (LList coll : removed.values()) {
			if (coll.contains(tuple)) {
				return true;
			}
		}

		return false;
	}

	public void restore(final int level) {
		final Iterator<Integer> itr = removed.keySet().iterator();

		while (itr.hasNext()) {
			final int currentLevel = itr.next();
			if (currentLevel >= level) {
				tuples.addAll(removed.get(currentLevel));
				itr.remove();
			}
		}

	}

	public void save(final int[] tuple, final int level) {

	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == false) {
			tuples.remove(tuple);
			for (LList t : removed.values()) {
				t.remove(tuple);
			}
		} else {
			tuples.add(tuple.clone());
		}
	}

	public TupleListDynamic clone() throws CloneNotSupportedException {
		final TupleListDynamic list = new TupleListDynamic();

		for (int[] t : tuples) {
			list.tuples.add(t);
		}

		list.removed = new HashMap<Integer, LList>();
		for (int level : removed.keySet()) {
			final LList llist = new LList();
			for (int[] t : removed.get(level)) {
				llist.add(t);
			}

			list.removed.put(level, llist);

		}

		// list.list = new HashSet<BigInteger>(this.list);

		return list;
	}

	@Override
	public LLIterator iterator() {
		return tuples.iterator();
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();

		for (int[] tuple : this) {
			stb.append(Arrays.toString(tuple)).append(",");
		}
		return stb.toString();
	}

	public boolean noRemaining(
			ExtensionConstraintGeneral extensionConstraintGeneral) {
		for (LList tuples : this.removed.values()) {
			for (int[] t : tuples) {
				assert !extensionConstraintGeneral.controlTuplePresence(t, -1) : Arrays
						.toString(t)
						+ " should have been restored";

			}
		}
		return true;
	}

	public class LList implements Iterable<int[]> {
		private Cell first;
		private Cell last;

		public LList() {
			first = last = null;
		}

		public void add(final int[] tuple) {
			addCell(new Cell(tuple));
		}

		public void addAll(final LList list) {
			assert list.last != null;

			if (first == null) {
				first = list.first;
				last = list.last;
			} else {
				last.next = list.first;
				list.first.prev = last;
				last = list.last;
			}
		}

		public void addCell(final Cell cell) {
			if (last == null) {
				first = last = cell;
				cell.next=null;
				cell.prev=null;
			} else {
				last.add(cell);
				last = cell;
			}

		}

		public boolean contains(final int[] tuple) {

			for (int[] t : this) {
				if (Arrays.equals(t, tuple)) {
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

		public String toString() {
			return first.toString();
		}

		@Override
		public LLIterator iterator() {
			return new LLIterator();
		}

		public class LLIterator implements Iterator<int[]> {

			private Cell current;

			public LLIterator() {
				current = first;
//				assert first != null;
//				assert last != null;
			}

			@Override
			public boolean hasNext() {

				return current != null;
			}

			@Override
			public int[] next() {
				final int[] tuple = current.tuple;

				current = current.next;

				return tuple;
			}

			@Override
			public void remove() {

			}

			public void remove(final int level) {
				final Cell toDelete;
				if (current == null) {
					toDelete = last;
				} else {
					toDelete = current.prev;
				}
				if (toDelete.prev == null) {
					first = toDelete.next;
				}
				if (toDelete.next == null) {
					last = toDelete.prev;
				}
				toDelete.remove();

				LList rmv = removed.get(level);
				if (rmv == null) {
					rmv = new LList();
					removed.put(level, rmv);
				}
				rmv.addCell(toDelete);
			}

		}
	}

	private static class Cell {
		private final int[] tuple;
		private Cell next;
		private Cell prev;

		public Cell(final int[] tuple) {
			this.tuple = tuple;
			next = prev = null;
		}

		public void add(final Cell cell) {
			next = cell;
			cell.prev = this;
			cell.next = null;
		}

		public void remove() {
			if (prev != null) {
				prev.next = next;
			}
			if (next != null) {
				next.prev = prev;
			}
		}

		public boolean contains(final int[] tuple) {
			if (Arrays.equals(this.tuple, tuple)) {
				return true;
			}
			if (next == null) {
				return false;
			}
			return next.contains(tuple);
		}

		public String toString() {
			if (next == null) {
				return Arrays.toString(tuple);
			}
			return Arrays.toString(tuple) + "," + next.toString();
		}

	}

	// public static void main(String[] args) {
	// final LList tuples = new LList();
	//
	// tuples.add(new int[] { 0, 4, 1 });
	// // tuples.add(new int[] { 0, 1, 1 });
	// // tuples.add(new int[] { 0, 0, 2 });
	// // tuples.add(new int[] { 0, 3, 1 });
	// // tuples.add(new int[] { 1, 0, 1 });
	// // tuples.add(new int[] { 0, 0, 4 });
	// // tuples.add(new int[] { 0, 2, 1 });
	//
	// final LList t2 = new LList();
	//
	// t2.add(new int[] { 2, 4, 1 });
	// t2.add(new int[] { 2, 1, 1 });
	// t2.add(new int[] { 2, 0, 2 });
	//
	// final Iterator<int[]> itr1 = tuples.iterator();
	// while (itr1.hasNext()) {
	// final int[] tuple = itr1.next();
	// if (tuple[1] == 4)
	// itr1.remove();
	//
	// }
	//
	// tuples.addAll(t2);
	//
	// final Iterator<int[]> itr = tuples.iterator();
	// while (itr.hasNext()) {
	// final int[] tuple = itr.next();
	// System.out.print(Arrays.toString(tuple));
	// }
	//
	// System.out.println();
	//
	// System.out.println(Arrays.toString(tuples.first.tuple));
	// System.out.println(Arrays.toString(tuples.last.tuple));
	// }

}
