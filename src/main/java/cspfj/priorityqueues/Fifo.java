package cspfj.priorityqueues;

import java.util.Arrays;
import java.util.LinkedList;

/**
 * Very simple FIFO queue based on LinkedList. All Identified elements are
 * unique.
 * 
 * @author scand1sk
 * 
 * @param <T>
 */
public class Fifo<T extends Identified> extends LinkedList<T> {

	private static final long serialVersionUID = 1L;

	private boolean[] inQueue;

	public Fifo(final int initSize) {
		inQueue = new boolean[initSize];
	}

	/**
	 * Increases the capacity of this instance, if necessary, to ensure that it
	 * can hold at least the number of elements specified by the minimum
	 * capacity argument.
	 * 
	 * @param minCapacity
	 *            the desired minimum capacity
	 */
	private void ensureCapacity(int minCapacity) {
		int oldCapacity = inQueue.length;
		if (minCapacity > oldCapacity) {
			int newCapacity = (oldCapacity * 3) / 2 + 1;
			if (newCapacity < minCapacity) {
				newCapacity = minCapacity;
			}
			// minCapacity is usually close to size, so this is a win:
			inQueue = Arrays.copyOf(inQueue, newCapacity);
		}
	}

	@Override
	public boolean offer(T e) {
		final int id = e.getId();
		ensureCapacity(id + 1);
		if (inQueue[id]) {
			return false;
		}
		inQueue[id] = true;
		return super.offer(e);
	}

	@Override
	public T poll() {
		final T data = super.poll();
		inQueue[data.getId()] = false;
		return data;
	}

	public void clear() {
		Arrays.fill(inQueue, false);
		super.clear();
	}

}
