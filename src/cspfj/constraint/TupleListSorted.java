package cspfj.constraint;

import java.util.Comparator;
import java.util.NavigableSet;
import java.util.TreeSet;

public class TupleListSorted implements Matrix, Cloneable {

	private final boolean initialContent;

	private final NavigableSet<int[]> tuples;

	public TupleListSorted(final boolean initialContent) {
		this.initialContent = initialContent;
		tuples = new TreeSet<int[]>(new TupleComparator());
	}

	@Override
	public boolean check(final int[] tuple) {
		return tuples.contains(tuple) ^ initialContent;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == initialContent) {
			tuples.remove(tuple);
		} else {
			tuples.add(tuple.clone());
		}
	}

	public boolean getInitialContent() {
		return initialContent;
	}
	
	public TupleListSorted clone() throws CloneNotSupportedException {
		final TupleListSorted list = (TupleListSorted) super.clone();

		// list.list = new HashSet<BigInteger>(this.list);

		return list;
	}
	
	public int[] first() {
		return tuples.first();
	}
	
	public int[] higher(int[] tuple) {
		return tuples.higher(tuple);
	}

	private static class TupleComparator implements Comparator<int[]> {

		@Override
		public int compare(int[] o1, int[] o2) {
			for (int i = 0; i < o1.length; i++) {
				if (o1[i] < o2[i]) {
					return -1;
				}
				if (o1[i] > o2[i]) {
					return 1;
				}
			}
			return 0;
		}

	}

	public static void main(String[] args) {

	}

}
