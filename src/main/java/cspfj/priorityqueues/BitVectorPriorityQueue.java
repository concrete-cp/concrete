package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Comparator;
import java.util.Iterator;

import cspfj.util.BitVector;

public class BitVectorPriorityQueue<T extends Identified> extends
        AbstractQueue<T> {

    private final BitVector queue;

    private final T[] values;

    private final Key<T> key;

    public BitVectorPriorityQueue(final Key<T> key, final T[] values) {
        this.values = values;
        queue = BitVector.factory(values.length, false);
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
        int bestKey = key.getKey(values[best]);
        for (int i = queue.nextSetBit(best + 1); i >= 0; i = queue
                .nextSetBit(i + 1)) {
            final int keyValue = key.getKey(values[i]);

            if (keyValue < bestKey) {
                best = i;
                bestKey = keyValue;
            }
        }

        return best;
    }
}
