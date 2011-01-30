package cspfj.util;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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

    @Override
    public Integer get(final int index) {
        throw new UnsupportedOperationException();
    }

}
