package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.Iterator;
import cspfj.constraint.Constraint;

/**
 * Very simple FIFO queue based on LinkedList. All Identified elements are
 * unique.
 * 
 * @author scand1sk
 * 
 * @param <T>
 */
public final class JavaSimpleFifos extends AbstractQueue<Constraint> {

    private static final int DEFAULT_INIT_SIZE = 10;

    private static final int NB_LISTS = 8;

    private Cell[] inQueue;

    private final MyLinkedList[] lists = new MyLinkedList[NB_LISTS];

    private int iter = 0;

    private int first = NB_LISTS;
    private int last = 0;

    // public static int insert = 0;
    // public static int update = 0;
    // public static int remove = 0;

    public JavaSimpleFifos(final Key<?> k) {
        this(DEFAULT_INIT_SIZE);
    }

    public JavaSimpleFifos(final int initSize) {
        inQueue = new Cell[initSize];
        for (int i = NB_LISTS; --i >= 0;) {
            lists[i] = new MyLinkedList();
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
    public boolean offer(final Constraint e) {
        final int id = e.getId();
        ensureCapacity(id + 1);
        Cell currentCell = inQueue[id];
        if (currentCell == null) {
            currentCell = new Cell(e);
            inQueue[id] = currentCell;
        }
        final int list = e.simpleEvaluation();
        if (currentCell.isPresent(iter)) {
            final MyLinkedList currentList = currentCell.myList;
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
    public Constraint poll() {
        for (int i = first; i <= last; i++) {
            if (!lists[i].isEmpty()) {
                // remove++;
                first = i;
                final Constraint val = lists[i].poll();
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
    public Iterator<Constraint> iterator() {
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
    public Constraint peek() {
        for (int i = first; i <= last; i++) {
            if (!lists[i].isEmpty()) {
                return lists[i].peek();
            }
        }
        return null;
    }

    private static final class Cell {
        private final Constraint content;
        private Cell prev;
        private Cell next;
        private MyLinkedList myList;
        private int inQueue = -1;

        private Cell(final Constraint content) {
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

    private static final class MyLinkedList {

        private int size;

        private Cell head;

        private MyLinkedList() {
            head = new Cell(null);
            head.myList = this;
            clear();
        }

        private void offer(final Cell cell, final int newIter) {
            cell.prev = head.prev;
            cell.next = head;
            head.prev.next = cell;
            head.prev = cell;
            cell.myList = this;
            cell.setPresent(newIter);
            size++;
        }

        private void remove(final Cell cell) {
            cell.next.prev = cell.prev;
            cell.prev.next = cell.next;
            cell.unsetPresent();
            cell.myList = null;
            size--;
        }

        private Constraint poll() {
            final Constraint contents = head.next.content;
            remove(head.next);
            return contents;
        }

        private Constraint peek() {
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
            for (Cell cell = head.next.next; cell != head; cell = cell.next) {
                stb.append(", ").append(cell.content);
            }
            return stb.append('}').toString();
        }

    }

}
