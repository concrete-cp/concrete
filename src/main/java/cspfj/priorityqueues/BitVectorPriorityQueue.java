package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Iterator;

public final class BitVectorPriorityQueue<T extends Identified> extends
		AbstractQueue<T> {

	private final BitSet queue;

	private T[] values;

	private final Key<T> key;

	public BitVectorPriorityQueue(final Key<T> key) {
		this(key, 10);
	}

	public BitVectorPriorityQueue(final Key<T> key, final int initSize) {
		this.values = (T[]) new Identified[initSize];
		queue = new BitSet(initSize);
		this.key = key;
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
		}
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
	public boolean offer(final T e) {
		final int id = e.getId();
		ensureCapacity(id + 1);
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
		float bestKey = key.getKey(values[best]);
		for (int i = queue.nextSetBit(best + 1); i >= 0; i = queue
				.nextSetBit(i + 1)) {
			final float keyValue = key.getKey(values[i]);

			if (keyValue < bestKey) {
				best = i;
				bestKey = keyValue;
			}
		}

		return best;
	}
}
