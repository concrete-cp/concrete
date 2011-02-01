package cspfj.problem;

import java.util.Iterator;

public class DomainIterator implements Iterator<Integer> {
    private final Domain domain;
    private int index;

    public DomainIterator(final Domain domain) {
        this.domain = domain;
        this.index = domain.first();
    }

    @Override
    public boolean hasNext() {
        return index >= 0;
    }

    @Override
    public Integer next() {
        final int current = index;
        index = domain.next(index);
        return current;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

}
