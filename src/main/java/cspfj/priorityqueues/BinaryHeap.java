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

    private T[] content;

    private int[] inQueue;

    private int size;

    private final Key<T> key;

    public BinaryHeap(final Key<T> key) {
        this(key, 10);
    }

    public BinaryHeap(final Key<T> key, final int initSize) {
        super();
        this.key = key;
        this.inQueue = new int[initSize];
        this.content = (T[]) new Identified[initSize];
        Arrays.fill(inQueue, -1);
        size = 0;
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
        assert content.length == oldCapacity;
        if (minCapacity > oldCapacity) {
            final int newCapacity = Math.max(minCapacity,
                    (oldCapacity * 3) / 2 + 1);
            // minCapacity is usually close to size, so this is a win:
            inQueue = Arrays.copyOf(inQueue, newCapacity);
            Arrays.fill(inQueue, oldCapacity, newCapacity, -1);
            content = Arrays.copyOf(content, newCapacity);
        }
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean offer(final T arg0) {
        final int id = arg0.getId();
        ensureCapacity(id + 1);
        final int initPos = inQueue[id];
        if (initPos >= 0) {
            siftUp(initPos);
            final int newPos = inQueue[id];
            if (initPos == newPos) {
                siftDown(inQueue[id]);
            }
            return false;
        }

        content[size] = arg0;
        inQueue[id] = size;
        siftUp(size++);
        return true;
    }

    @Override
    public T poll() {
        switch (size) {
        case 0:
            throw new NoSuchElementException();
        case 1:
            size = 0;
            inQueue[content[0].getId()] = -1;
            return content[0];
        default:
            final T max = content[0];
            content[0] = content[--size];
            siftDown(0);
            inQueue[max.getId()] = -1;
            return max;
        }
    }

    @Override
    public T peek() {
        if (size == 0) {
            throw new NoSuchElementException();
        }
        return content[0];
    }

    @Override
    public void clear() {
        size = 0;
        Arrays.fill(inQueue, -1);
    }

    @Override
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    private void swap(final int int0, final int int1) {
        final T tmp = content[int0];
        content[int0] = content[int1];
        content[int1] = tmp;
        inQueue[content[int0].getId()] = int0;
        inQueue[content[int1].getId()] = int1;
    }

    private void siftDown(final int start) {
        int root = start;
        final int end = size - 1;
        int child;
        while ((child = (root << 1) + 1) <= end) {
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
            final int parent = ((leaf - 1) >> 1);

            if (key.getKey(content[parent]) > key.getKey(content[leaf])) {
                swap(parent, leaf);
                leaf = parent;
            } else {
                return;
            }
        }
        return;
    }

    public String toString() {
        return Arrays.toString(content);
    }

    public boolean control(final int position) {
        if (position >= size) {
            return true;
        }
        int child1 = (position << 1) + 1;
        int child2 = (position << 1) + 2;
        final float thisKey = key.getKey(content[position]);

        if (child1 < size && thisKey > key.getKey(content[child1])) {
            return false;
        }
        if (child2 < size && thisKey > key.getKey(content[child2])) {
            return false;
        }
        return control(child1) && control(child2);

    }
}
