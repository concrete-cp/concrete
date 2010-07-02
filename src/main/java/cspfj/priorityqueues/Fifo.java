package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.Iterator;

/**
 * Very simple FIFO queue based on LinkedList. All Identified elements are
 * unique.
 * 
 * @author scand1sk
 * 
 * @param <T>
 */
public final class Fifo<T extends Identified> extends AbstractQueue<T> {

	private static final int DEFAULT_NB_LISTS = 8;

	private static final int KEY_FACTOR = 8;

	private static final long serialVersionUID = 1L;

	private Cell<T>[] inQueue;

	private final MyLinkedList<T>[] lists;

	private final int nbLists;

	private final Key<T> key;

//	public static int insert = 0;
//	public static int update = 0;
//	public static int remove = 0;

	public Fifo(final Key<T> k) {
		this(k, DEFAULT_NB_LISTS);
	}

	public Fifo(final Key<T> k, final int nbLists) {
		this(k, nbLists, 10);
	}

	public Fifo(final Key<T> k, final int nbLists, final int initSize) {
		inQueue = (Cell<T>[]) new Cell[initSize];
		lists = (MyLinkedList<T>[]) new MyLinkedList[nbLists];
		for (int i = nbLists; --i >= 0;) {
			lists[i] = new MyLinkedList<T>();
		}
		this.key = k;
		this.nbLists = nbLists;

	}

	private int chooseList(final T element) {
		if (nbLists <= 1) {
			return 0;
		}
		final float k = key.getKey(element);
		for (int i = 0, treshold = KEY_FACTOR;; i++, treshold *= KEY_FACTOR) {
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
		Cell<T> currentCell = inQueue[id];
		if (currentCell == null) {
			currentCell = new Cell<T>(e);
			inQueue[id] = currentCell;
		}
		final int list = chooseList(e);
		if (currentCell.myList == lists[list]) {
			return false;
		}
		if (currentCell.myList != null) {
			currentCell.myList.remove(currentCell);
//			update++;
		} 
//		else {
//			insert++;
//		}

		lists[list].offer(currentCell);
		return true;
	}

	@Override
	public T poll() {
		for (MyLinkedList<T> ll : lists) {
			if (!ll.isEmpty()) {
//				remove++;
				return ll.poll();
			}
		}
		return null;
	}

	@Override
	public void clear() {
		for (Cell<T> cell : inQueue) {
			if (cell != null) {
				cell.myList = null;
			}
		}
		for (MyLinkedList<T> ll : lists) {
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
		for (MyLinkedList<T> ll : lists) {
			size += ll.size();
		}
		return size;
	}

	@Override
	public boolean isEmpty() {
		for (MyLinkedList<T> ll : lists) {
			if (!ll.isEmpty()) {
				return false;
			}
		}
		return true;
	}

	@Override
	public T peek() {
		for (MyLinkedList<T> ll : lists) {
			if (!ll.isEmpty()) {
				return ll.peek();
			}
		}
		return null;
	}

	private static final class Cell<T> {
		private final T content;
		private Cell<T> prev;
		private Cell<T> next;
		private MyLinkedList<T> myList;

		private Cell(final T content) {
			this.content = content;
			this.prev = null;
			this.next = null;
			this.myList = null;
		}
	}

	private static final class MyLinkedList<T> {

		private int size;

		private Cell<T> head;
		private Cell<T> tail;

		private MyLinkedList() {
			size = 0;
			head = null;
			tail = null;
		}

		private void offer(final Cell<T> cell) {
			cell.prev = null;
			cell.next = head;
			if (head != null) {
				head.prev = cell;
			}
			head = cell;
			if (tail == null) {
				tail = cell;
			}
			cell.myList = this;
			size++;
		}

		private void remove(final Cell<T> cell) {
			if (cell.next == null) {
				tail = cell.prev;
				if (tail == null) {
					head = null;
				} else {
					tail.next = null;
				}
			} else if (cell.prev == null) {
				head = cell.next;
				if (head == null) {
					tail = null;
				} else {
					head.prev = null;
				}

			} else {
				cell.next.prev = cell.prev;
				cell.prev.next = cell.next;
			}
			cell.myList = null;
			size--;
		}

		private T poll() {
			final T contents = tail.content;
			tail.myList = null;

			tail = tail.prev;
			if (tail == null) {
				head = null;
			} else {
				tail.next = null;
			}
			size--;
			return contents;
		}

		private T peek() {
			return tail.content;
		}

		public int size() {
			return size;
		}

		public boolean isEmpty() {
			return head == null;
		}

		public void clear() {
			head = null;
			tail = null;
			size = 0;
		}

		public String toString() {
			if (isEmpty()) {
				return "{}";
			}
			final StringBuilder stb = new StringBuilder();
			stb.append('{').append(head.content);
			for (Cell<T> cell = head.next; cell != null; cell = cell.next) {
				stb.append(", ").append(cell.content);
			}
			return stb.append('}').toString();
		}

	}

}
