package cspfj.constraint.extension;

import java.util.Iterator;

import cspfj.util.IntTupleSet;

public final class TupleSet implements Matrix, Iterable<int[]> {

    private IntTupleSet tupleSet;

    private final boolean initContent;

    private TupleSet(final IntTupleSet tupleSet, final boolean initialContent) {
        super();
        this.initContent = initialContent;
        this.tupleSet = tupleSet;
    }

    public TupleSet(final int nbTuples, final boolean initialContent) {
        this(new IntTupleSet(nbTuples), initialContent);
    }

    public TupleSet(final boolean initialContent) {
        this(new IntTupleSet(), initialContent);
    }

    @Override
    public TupleSet clone() throws CloneNotSupportedException {
        final TupleSet clone = (TupleSet) super.clone();
        clone.tupleSet = new IntTupleSet(tupleSet);
        return clone;
    }

    @Override
    public boolean check(final int[] tuple) {
        return tupleSet.containsTuple(tuple) ^ initContent;
    }

    @Override
    public void set(final int[] tuple, final boolean status) {
        if (status == initContent) {
            tupleSet.remove(tuple);
        } else {
            tupleSet.add(tuple.clone());
        }
    }

    public boolean getInitialContent() {
        return initContent;
    }

    @Override
    public boolean isEmpty() {
        return tupleSet.isEmpty() && !initContent;
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
        if (initContent) {
            return "nogoods: " + tupleSet.toString();
        }
        return "goods: " + tupleSet.toString();
    }
}
