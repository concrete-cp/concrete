package cspfj.constraint.extension;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class TupleHashSet implements Matrix, Cloneable, Iterable<int[]> {

	// private Map<Long, Collection<int[]>> list;

	private final boolean initialContent;

	private TupleSet set;

	private List<int[]> list;

	public TupleHashSet(final boolean initialContent) {
		this(initialContent, 12);

	}

	public TupleHashSet(final boolean initialContent, final int nbTuples) {
		this.initialContent = initialContent;
		// list = new HashMap<Long, Collection<int[]>>();
		set = new TupleSet(nbTuples);
		list = new ArrayList<int[]>();

		// System.out.println("Hashtable of " + hashTableSize + " buckets");

	}

	public boolean getInitialContent() {
		return initialContent;
	}

	@Override
	public boolean check(final int[] tuple) {
		return set.containsTuple(tuple) ^ initialContent;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == initialContent) {
			set.removeTuple(tuple);
			for (int i = list.size(); --i >= 0;) {
				if (Arrays.equals(list.get(i), tuple)) {
					list.set(i, null);
				}
			}
		} else {
			final int[] clone = tuple.clone();
			set.add(clone);
			list.add(clone);
		}
	}

	public int[] getTuple(final int index) {
		return list.get(index);
	}

	public int getNbTuples() {
		return list.size();
	}

	public TupleHashSet clone() throws CloneNotSupportedException {
		final TupleHashSet ths = (TupleHashSet) super.clone();

		ths.list = new ArrayList<int[]>(list);
		ths.set = (TupleSet) set.clone();

		// list.list = new HashSet<BigInteger>(this.list);

		return ths;
	}

	public static boolean isBetterThanMatrix(final int[] sizes,
			final int nbTuples) {
		BigInteger size = BigInteger.ONE;
		for (int s : sizes) {
			size = size.multiply(BigInteger.valueOf(s));
		}

		return size.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) >= 0
				|| (4 * nbTuples < size.intValue());
	}

	@Override
	public Iterator<int[]> iterator() {
		return set.iterator();
	}

}
