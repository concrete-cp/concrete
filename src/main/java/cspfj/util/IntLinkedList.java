package cspfj.util;

import java.util.AbstractList;
import java.util.List;
import java.util.ListIterator;

public final class IntLinkedList extends AbstractList<Integer> {

	private Entry head = null;

	private Entry queue = null;

	private int size = 0;

	public void add(int value) {
		final Entry newValue = new Entry(value, head);
		if (head == null) {
			queue = newValue;
		}
		head = newValue;
		size++;
	}

	public void addAll(IntLinkedList list) {
		if (list.head == null) {
			return;
		}
		list.queue.next = head;
		head = list.head;
	}

	public int size() {
		return size;
	}

	public boolean isEmpty() {
		return head == null;
	}

	public int max() {
		int max = head.value;
		for (Entry e = head.next; e != null; e = e.next) {
			max = Math.max(max, e.value);
		}
		return max;
	}

	public void clear() {
		head = null;
		queue = null;
		size = 0;
	}

	private final static class Entry {
		private Entry next;
		private final int value;

		public Entry(int value, Entry next) {
			this.value = value;
			this.next = next;
		}
	}

	public static int[] intListToArray(final List<Integer> list) {
		final int[] array = new int[list.size()];
		for (final ListIterator<Integer> itr = list.listIterator(); itr
				.hasNext();) {
			array[itr.nextIndex()] = itr.next();
		}
		return array;
	}

	@Override
	public Integer get(int index) {
		throw new UnsupportedOperationException();
	}
}
