package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Iterator;

/**
 * Very simple FIFO queue based on LinkedList. All Identified elements are
 * unique.
 * 
 * @author scand1sk
 * 
 * @param <T>
 */
public final class Array<T extends Identified> extends AbstractQueue<T> {

	private static final long serialVersionUID = 1L;

	private final BitSet inQueue;

	private T[] contents;

	private final Key<T> key;

	private int size;

	public Array(final Key<T> key) {
		this(10, key);
	}

	public Array(final int initSize, final Key<T> key) {
		inQueue = new BitSet(initSize);
		contents = (T[]) new Identified[initSize];
		this.key = key;
		size = 0;
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
		int oldCapacity = contents.length;
		if (minCapacity > oldCapacity) {
			final int newCapacity = Math.max(minCapacity,
					(oldCapacity * 3) / 2 + 1);
			// minCapacity is usually close to size, so this is a win:
			contents = Arrays.copyOf(contents, newCapacity);
		}
	}

	@Override
	public boolean offer(final T e) {
		final int id = e.getId();
		ensureCapacity(id + 1);
		if (inQueue.get(id)) {
			return false;
		}
		inQueue.set(id);
		contents[id] = e;
		size++;
		return true;
	}

	@Override
	public T poll() {
		final int best = best();
		inQueue.clear(best);
		size--;
		return contents[best];
	}

	public void clear() {
		inQueue.clear();
	}

	@Override
	public Iterator<T> iterator() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		return size;
	}

	@Override
	public T peek() {
		return contents[best()];
	}

	private int best() {
		double bestKey = Double.POSITIVE_INFINITY;
		int best = -1;
		for (int i = inQueue.nextSetBit(0); i >= 0; i = inQueue
				.nextSetBit(i + 1)) {
			final double k = key.getKey(contents[i]);
			if (k < bestKey) {
				bestKey = k;
				best = i;
			}
		}
		return best;
	}

}
