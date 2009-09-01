/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj.priorityqueues;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Simple array-based, auto-expandable binary heap implementation. Stores
 * Identified objects. Each uniquely identified object can be present only once
 * in the queue.
 * 
 * @author scand1sk
 * 
 * @param <T>
 */
public final class BinaryHeap<T extends Identified> extends AbstractQueue<T> {

    private static final int ARRAY_INCREASE = 64;

    private T[] content;

    private boolean[] inQueue;

    private int size;

    private final Key<T> key;

    public BinaryHeap(final Key<T> key, final int initSize) {
        super();
        this.key = key;
        this.inQueue = new boolean[initSize];
        this.content = (T[]) new Identified[initSize];
        size = 0;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean offer(final T arg0) {
        final int id = arg0.getId();
        if (id >= content.length) {
            content = Arrays.copyOf(content, id + ARRAY_INCREASE);
            inQueue = Arrays.copyOf(inQueue, id + ARRAY_INCREASE);
        } else if (inQueue[id]) {
            return false;
        }
        inQueue[arg0.getId()] = true;
        content[size] = arg0;
        siftUp(size++);
        return true;
    }

    @Override
    public T poll() throws NoSuchElementException {
        switch (size) {
        case 0:
            throw new NoSuchElementException();
        case 1:
            size--;
            inQueue[content[0].getId()] = false;
            return content[0];
        default:
            final T max = content[0];
            content[0] = content[--size];
            siftDown(0);
            inQueue[max.getId()] = false;
            return max;
        }
    }

    @Override
    public T peek() throws NoSuchElementException {
        if (size == 0) {
            throw new NoSuchElementException();
        }
        return content[0];
    }

    @Override
    public void clear() {
        size = 0;
        Arrays.fill(inQueue, false);
    }

    @Override
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    private void swap(final int int0, final int int1) {
        final T tmp = content[int0];
        content[int0] = content[int1];
        content[int1] = tmp;
    }

    private void siftDown(final int start) {
        int root = start;
        final int end = size - 1;
        while ((root << 1) + 1 <= end) {
            int child = (root << 1) + 1;

            if (child < end
                    && key.getKey(content[child]) > key
                            .getKey(content[child + 1])) {
                child++;
            }
            if (key.getKey(content[root]) > key.getKey(content[child])) {
                swap(root, child);
                root = child;
            } else {
                return;
            }
        }
    }

    private void siftUp(final int start) {
        int leaf = start;
        while (leaf > 0) {
            int parent = ((leaf - 1) >> 1);

            if (key.getKey(content[parent]) > key.getKey(content[leaf])) {
                swap(parent, leaf);
                leaf = parent;
            } else {
                return;
            }
        }
    }

    public String toString() {
        return Arrays.toString(content);
    }

}
