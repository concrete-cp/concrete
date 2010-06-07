package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.BitSet;
import java.util.Iterator;

public final class BitVectorPriorityQueue<T extends Identified> extends
        AbstractQueue<T> {

    private final BitSet queue;

    private final T[] values;

    private final Key<T> key;

    public BitVectorPriorityQueue(final Key<T> key, final int initSize) {
        this.values = (T[]) new Identified[initSize];
        queue = new BitSet(initSize);
        this.key = key;
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
        final int id = e.getId();
        values[id] = e;
        queue.set(id);
        return true;
    }

    @Override
    public T peek() {
        return values[min()];
    }

    @Override
    public void clear() {
        queue.clear();
    }

    @Override
    public T poll() {
        final int min = min();
        queue.set(min, false);
        return values[min];
    }

    private int min() {
        int best = queue.nextSetBit(0);
        double bestKey = key.getKey(values[best]);
        for (int i = queue.nextSetBit(best + 1); i >= 0; i = queue
                .nextSetBit(i + 1)) {
            final double keyValue = key.getKey(values[i]);

            if (keyValue < bestKey) {
                best = i;
                bestKey = keyValue;
            }
        }

        return best;
    }
}
