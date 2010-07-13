package cspfj.constraint.extension;

import java.util.Iterator;

import cspfj.util.IntTupleSet;

public final class TupleSet implements Matrix, Cloneable, Iterable<int[]> {

    private IntTupleSet tupleSet;

    private final boolean initialContent;

    private TupleSet(final IntTupleSet tupleSet, final boolean initialContent) {
        super();
        this.initialContent = initialContent;
        this.tupleSet = tupleSet;
    }

    public TupleSet(final int nbTuples, final boolean initialContent) {
        this(new IntTupleSet(nbTuples), initialContent);
    }

    public TupleSet(final boolean initialContent) {
        this(new IntTupleSet(), initialContent);
    }

    @Override
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
    }

    public boolean getInitialContent() {
        return initialContent;
    }

    @Override
    public boolean isEmpty() {
        return tupleSet.isEmpty() && !initialContent;
    }

    public int size() {
        return tupleSet.size();
    }

    @Override
    public Iterator<int[]> iterator() {
        return tupleSet.iterator();
    }

    @Override
    public String toString() {
        if (initialContent) {
            return "nogoods: " + tupleSet.toString();
        }
        return "goods: " + tupleSet.toString();
    }
}
