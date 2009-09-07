package cspfj.util;

public final class IntLinkedList {

	private Entry head = null;

	private Entry queue = null;

	private int size = 0;

	// public IntLinkedList() {
	//
	// }

	public void add(int value) {
		final Entry newValue = new Entry(value, head);
		if (head == null) {
			queue = newValue;
		}
		head = newValue;
		size++;
	}

	public void stick(IntLinkedList list) {
		if (list.head == null) {
			return;
		}
		list.queue.next = head;
		head = list.head;
	}

	public int getSize() {
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
}
