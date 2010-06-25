package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * Very simple FIFO queue based on LinkedList. All Identified elements are
 * unique.
 * 
 * @author scand1sk
 * 
 * @param <T>
 */
public final class Fifo<T extends Identified> extends AbstractQueue<T> {

	private static final long serialVersionUID = 1L;

	private boolean[] inQueue;

	private final List<LinkedList<T>> list;

	private final int nbLists;

	private final Key<T> key;

	public Fifo(final Key<T> k) {
		this(k, 5);
	}

	public Fifo(final Key<T> k, final int nbLists) {
		this(k, nbLists, 10);
	}

	public Fifo(final Key<T> k, final int nbLists, final int initSize) {
		inQueue = new boolean[initSize];
		list = new ArrayList<LinkedList<T>>(nbLists);
		for (int i = nbLists; --i >= 0;) {
			list.add(new LinkedList<T>());
		}
		this.key = k;
		this.nbLists = nbLists;

	}

	private int chooseList(final T element) {
		if (nbLists <= 1) {
			return 0;
		}
		final double k = key.getKey(element);
		if (k < 100) {
			return 0;
		}
		for (int i = 1, treshold = 1000;; i++, treshold *= 10) {
			if (nbLists <= i + 1 || k < treshold) {
				return i;
			}
		}

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
		int oldCapacity = inQueue.length;
		if (minCapacity > oldCapacity) {
			final int newCapacity = Math.max(minCapacity,
					(oldCapacity * 3) / 2 + 1);
			// minCapacity is usually close to size, so this is a win:
			inQueue = Arrays.copyOf(inQueue, newCapacity);
		}
	}

	@Override
	public boolean offer(final T e) {
		final int id = e.getId();
		ensureCapacity(id + 1);
		if (inQueue[id]) {
			return false;
		}
		inQueue[id] = true;
		return list.get(chooseList(e)).offer(e);
	}

	@Override
	public T poll() {
		for (LinkedList<T> ll : list) {
			if (!ll.isEmpty()) {
				final T data = ll.poll();
				inQueue[data.getId()] = false;
				return data;
			}
		}
		return null;
	}

	public void clear() {
		Arrays.fill(inQueue, false);
		for (LinkedList<T> ll : list) {
			ll.clear();
		}
	}

	@Override
	public Iterator<T> iterator() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int size() {
		int size = 0;
		for (LinkedList<T> ll : list) {
			size += ll.size();
		}
		return size;
	}

	@Override
	public boolean isEmpty() {
		for (LinkedList<T> ll : list) {
			if (!ll.isEmpty()) {
				return false;
			}
		}
		return true;
	}

	@Override
	public T peek() {
		for (LinkedList<T> ll : list) {
			if (!ll.isEmpty()) {
				return ll.peek();
			}
		}
		return null;
	}

}
