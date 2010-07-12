package cspfj.constraint.extension;

import java.util.Iterator;

import cspfj.util.IntTupleSet;

public final class TupleSet implements Matrix, Cloneable, Iterable<int[]> {

	private IntTupleSet tupleSet;

	private final boolean initialContent;

	private boolean empty;

	public TupleSet(final int nbTuples, final boolean initialContent) {
		super();
		tupleSet = new IntTupleSet(nbTuples);
		this.initialContent = initialContent;
		empty = !initialContent;
	}

	public TupleSet(final boolean initialContent) {
		this(16, initialContent);
	}

	public TupleSet clone() {
		final TupleSet clone;
		try {
			clone = (TupleSet) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new InternalError(e.toString());
		}
		clone.tupleSet = new IntTupleSet(tupleSet);
		return clone;
	}

	@Override
	public boolean check(final int[] tuple) {
		return tupleSet.containsTuple(tuple) ^ initialContent;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == initialContent) {
			tupleSet.remove(tuple);
		} else {
			tupleSet.add(tuple.clone());
		}
		empty = false;
	}

	public boolean getInitialContent() {
		return initialContent;
	}

	public boolean isEmpty() {
		return empty;
	}

	public int size() {
		return tupleSet.size();
	}

	public Iterator<int[]> iterator() {
		return tupleSet.iterator();
	}
}
