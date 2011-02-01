package cspfj.problem;

import java.util.Iterator;

public abstract class AbstractDomain implements Domain {

    @Override
    public Iterator<Integer> iterator() {
        return new DomainIterator();
    }

    public class DomainIterator implements Iterator<Integer> {
        private int index;

        public DomainIterator() {
            this.index = first();
        }

        @Override
        public boolean hasNext() {
            return index >= 0;
        }

        @Override
        public Integer next() {
            final int current = index;
            index = AbstractDomain.this.next(index);
            return current;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }

    }
}
