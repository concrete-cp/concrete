package cspfj.util;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;

public final class IntLinkedList extends AbstractList<Integer> {

    private Entry head = null;

    private Entry queue = null;

    private int size = 0;

    public void add(final int value) {
        final Entry newValue = new Entry(value, head);
        if (head == null) {
            queue = newValue;
        }
        head = newValue;
        size++;
    }

    public void addAll(final IntLinkedList list) {
        if (list.head == null) {
            return;
        }
        list.queue.next = head;
        head = list.head;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
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

    @Override
    public void clear() {
        head = null;
        queue = null;
        size = 0;
    }

    private static final class Entry {
        private Entry next;
        private final int value;

        private Entry(final int value, final Entry next) {
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

    public static List<Integer> arrayToIntList(final int[] array) {
        final List<Integer> list = new ArrayList<Integer>(array.length);
        for (int i : array) {
            list.add(i);
        }
        return list;
    }

    public static int[] intCollectionToArray(final Collection<Integer> list) {
        final int[] array = new int[list.size()];
        int i = 0;
        for (int v : list) {
            array[i++] = v;
        }
        return array;
    }

    @Override
    public Integer get(final int index) {
        throw new UnsupportedOperationException();
    }
}
