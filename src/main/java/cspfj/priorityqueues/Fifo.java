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

    private static final int DEFAULT_INIT_SIZE = 10;

    private static final long serialVersionUID = 1L;

    private Cell<T>[] inQueue;

    private final MyLinkedList<T> list;

    private int iter = 0;

    // public static int insert = 0;
    // public static int update = 0;
    // public static int remove = 0;

    public Fifo() {
        this(DEFAULT_INIT_SIZE);
    }

    @SuppressWarnings("unchecked")
    public Fifo(final int initSize) {
        inQueue = (Cell<T>[]) new Cell[initSize];
        list = new MyLinkedList<T>();
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
        if (currentCell.iter == iter) {
            return false;
        }
        // else {
        // insert++;
        // }

        list.offer(currentCell, iter);
        return true;
    }

    @Override
    public T poll() {
        return list.poll();
    }

    @Override
    public void clear() {
        iter++;
        list.clear();
    }

    @Override
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int size() {
        return list.size();
    }

    @Override
    public boolean isEmpty() {
        return list.isEmpty();
    }

    @Override
    public T peek() {
        return list.peek();
    }

    private static final class Cell<T> {
        private final T content;
        private Cell<T> prev;
        private Cell<T> next;
        private int iter = -1;

        private Cell(final T content) {
            this.content = content;
            this.prev = null;
            this.next = null;
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

        private void offer(final Cell<T> cell, final int newIter) {
            cell.prev = null;
            cell.next = head;
            if (head != null) {
                head.prev = cell;
            }
            head = cell;
            if (tail == null) {
                tail = cell;
            }
            cell.iter = newIter;
            size++;
        }

        private T poll() {
            final T contents = tail.content;

            tail.iter = -1;

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
