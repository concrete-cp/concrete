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
public final class JavaFifos<T extends Identified> extends AbstractQueue<T> {

    private static final int DEFAULT_INIT_SIZE = 10;

    private static final int NB_LISTS = 8;

    private Cell<T>[] inQueue;

    @SuppressWarnings("unchecked")
    private final MyLinkedList<T>[] lists = (MyLinkedList<T>[]) new MyLinkedList[NB_LISTS];

    private final Key<T> key;

    private int iter = 0;

    private int first = NB_LISTS;
    private int last = 0;

    // public static int insert = 0;
    // public static int update = 0;
    // public static int remove = 0;

    public JavaFifos(final Key<T> k) {
        this(k, DEFAULT_INIT_SIZE);
    }

    @SuppressWarnings("unchecked")
    public JavaFifos(final Key<T> k, final int initSize) {
        inQueue = (Cell<T>[]) new Cell[initSize];
        for (int i = NB_LISTS; --i >= 0;) {
            lists[i] = new MyLinkedList<T>();
        }
        this.key = k;

    }

    private int chooseList(final T element) {
        final int k = key.getKey(element);
        return Math.min(NB_LISTS - 1,
                NB_LISTS - Integer.numberOfLeadingZeros(k) / 4);
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
        if (currentCell.isPresent(iter)) {
            final MyLinkedList<T> currentList = currentCell.myList;
            assert currentList != null;
            if (currentList == lists[list]) {
                return false;
            }

            currentList.remove(currentCell);
            if (currentList.isEmpty()) {
                if (currentList == lists[last]) {
                    last--;
                }
                if (currentList == lists[first]) {
                    first++;
                }
            }

        }
        // else {
        // insert++;
        // }

        lists[list].offer(currentCell, iter);
        if (list < first) {
            first = list;
        }
        if (list > last) {
            last = list;
        }
        return true;
    }

    @Override
    public T poll() {
        for (int i = first; i <= last; i++) {
            if (!lists[i].isEmpty()) {
                // remove++;
                first = i;
                final T val = lists[i].poll();
                if (lists[i].isEmpty() && i == last) {
                    last = -1;
                }
                return val;
            }
        }
        return null;
    }

    @Override
    public void clear() {
        iter++;
        for (int i = first; i <= last; i++) {
            lists[i].clear();
        }
        first = NB_LISTS;
        last = -1;
    }

    @Override
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int size() {
        int size = 0;
        for (int i = first; i <= last; i++) {
            size += lists[i].size();
        }
        return size;
    }

    @Override
    public boolean isEmpty() {

        return first > last;
    }

    @Override
    public T peek() {
        for (int i = first; i <= last; i++) {
            if (!lists[i].isEmpty()) {
                return lists[i].peek();
            }
        }
        return null;
    }

    private static final class Cell<T> {
        private final T content;
        private Cell<T> prev;
        private Cell<T> next;
        private MyLinkedList<T> myList;
        private int inQueue = -1;

        private Cell(final T content) {
            this.content = content;
            this.prev = null;
            this.next = null;
            this.myList = null;
        }

        private void setPresent(final int iter) {
            inQueue = iter;
        }

        private void unsetPresent() {
            inQueue = -1;
        }

        private boolean isPresent(final int iter) {
            return inQueue == iter;
        }
    }

    private static final class MyLinkedList<T> {

        private int size;

        private Cell<T> head;

        private MyLinkedList() {
            head = new Cell<T>(null);
            head.myList = this;
            clear();
        }

        private void offer(final Cell<T> cell, final int newIter) {
            cell.prev = head.prev;
            cell.next = head;
            head.prev.next = cell;
            head.prev = cell;
            cell.myList = this;
            cell.setPresent(newIter);
            size++;
        }

        private void remove(final Cell<T> cell) {
            cell.next.prev = cell.prev;
            cell.prev.next = cell.next;
            cell.unsetPresent();
            cell.myList = null;
            size--;
        }

        private T poll() {
            final T contents = head.next.content;
            remove(head.next);
            return contents;
        }

        private T peek() {
            return head.next.content;
        }

        public int size() {
            return size;
        }

        public boolean isEmpty() {
            return head.next == head;
        }

        public void clear() {
            head.next = head;
            head.prev = head;
            size = 0;
        }

        public String toString() {
            if (isEmpty()) {
                return "{}";
            }
            final StringBuilder stb = new StringBuilder();
            stb.append('{').append(head.next.content);
            for (Cell<T> cell = head.next.next; cell != head; cell = cell.next) {
                stb.append(", ").append(cell.content);
            }
            return stb.append('}').toString();
        }

    }

}
