package cspfj.constraint.extension;

import java.math.BigInteger;
import java.util.Iterator;

public class TupleHashSet implements Matrix, Cloneable, Iterable<int[]> {

	// private Map<Long, Collection<int[]>> list;

	private final boolean initialContent;

	private TupleSet set;

	public TupleHashSet(final boolean initialContent) {
		this(initialContent, 12);

	}

	public TupleHashSet(final boolean initialContent, final int nbTuples) {
		this.initialContent = initialContent;
		// list = new HashMap<Long, Collection<int[]>>();
		set = new TupleSet(nbTuples);

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
		} else {
			set.add(tuple.clone());
		}
	}

	public int getNbTuples() {
		return set.size();
	}

	public TupleHashSet clone() {
		TupleHashSet ths;
		try {
			ths = (TupleHashSet) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new IllegalStateException(e);
		}

		ths.set = set.clone();

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
