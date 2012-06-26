package cspfj.priorityqueues;

import java.util.Arrays;
import java.util.BitSet;

public final class BitVectorPriorityQueue<T extends Identified> implements
        PriorityQueue<T> {

    private final BitSet queue;

    private T[] values;

    private int[] evals;


    public BitVectorPriorityQueue() {
        this(10);
    }

    @SuppressWarnings("unchecked")
    public BitVectorPriorityQueue(final int initSize) {
        this.values = (T[]) new Identified[initSize];
        evals = new int[initSize];
        queue = new BitSet(initSize);
    }

    /**
     * Increases the capacity of this instance, if necessary, to ensure that it
     * can hold at least the number of elements specified by the minimum
     * capacity argument.
     * 
     * @param minCapacity
     *            the desired minimum capacity
     */
    private void ensureCapacity(final int minCapacity) {
        int oldCapacity = values.length;

        if (minCapacity > oldCapacity) {
            final int newCapacity = Math.max(minCapacity,
                    (oldCapacity * 3) / 2 + 1);
            // minCapacity is usually close to size, so this is a win:
            values = Arrays.copyOf(values, newCapacity);
            evals = Arrays.copyOf(evals, newCapacity);
        }
    }

   
    @Override
    public boolean isEmpty() {
        return queue.isEmpty();
    }

    @Override
    public boolean offer(final T e, final int eval) {
        final int id = e.getId();
        ensureCapacity(id + 1);
        values[id] = e;
        evals[id] = eval;
        if (queue.get(id)) {
            return false;
        } else {
            queue.set(id);
            return true;
        }
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
        int bestKey = evals[best];
        for (int i = queue.nextSetBit(best + 1); i >= 0; i = queue
                .nextSetBit(i + 1)) {
            final int keyValue = evals[i];

            if (keyValue < bestKey) {
                best = i;
                bestKey = keyValue;
            }
        }

        return best;
    }
}
