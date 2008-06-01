package cspfj.constraint;

import java.util.Iterator;

public class TupleHashSet implements Matrix, Cloneable, Iterable<int[]> {

	// private Map<Long, Collection<int[]>> list;

	private final boolean initialContent;

	private final TupleSet set;

	public TupleHashSet(final boolean initialContent) {
		this(initialContent, 12);
	}

	public TupleHashSet(final boolean initialContent, final int nbTuples) {
		this.initialContent = initialContent;
		// list = new HashMap<Long, Collection<int[]>>();
		set = new TupleSet(nbTuples);
	
//		System.out.println("Hashtable of " + hashTableSize + " buckets");

	}


	@Override
	public boolean check(final int[] tuple) {
		return set.containsTuple(tuple)
				^ initialContent;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == initialContent) {
			set.removeTuple(tuple);
		} else {
			set.add(tuple.clone());
		}
	}

	public TupleHashSet clone() throws CloneNotSupportedException {
		final TupleHashSet list = (TupleHashSet) super.clone();

		// list.list = new HashSet<BigInteger>(this.list);

		return list;
	}

	@Override
	public Iterator<int[]> iterator() {
		return set.iterator();
	}

}
