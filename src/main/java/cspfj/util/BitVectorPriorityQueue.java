package cspfj.util;

import java.util.AbstractQueue;
import java.util.Comparator;
import java.util.Iterator;

public class BitVectorPriorityQueue<T extends Identified> extends
        AbstractQueue<T> {

    private final BitVector queue;

    private final T[] values;

    private final Comparator<T> comparator;

    public BitVectorPriorityQueue(final Comparator<T> comparator,
            final T[] values) {
        this.values = values;
        queue = BitVector.factory(values.length, false);
        this.comparator = comparator;
    }

    @Override
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isEmpty() {
        return queue.isEmpty();
    }

    @Override
    public int size() {
        return queue.cardinality();
    }

    @Override
    public boolean offer(T e) {
        return queue.set(e.getId());
    }

    @Override
    public T peek() {
        return values[min()];
    }

    @Override
    public void clear() {
        queue.fill(false);
    }

    @Override
    public T poll() {
        final int min = min();
        queue.set(min, false);
        return values[min];
    }

    private int min() {
        int best = queue.nextSetBit(0);
        T bestValue = values[best];
        for (int i = queue.nextSetBit(best + 1); i >= 0; i = queue
                .nextSetBit(i + 1)) {
            final T elt = values[i];

            if (comparator.compare(elt, bestValue) < 0) {
                best = i;
                bestValue = elt;
            }
        }

        return best;
    }
}
