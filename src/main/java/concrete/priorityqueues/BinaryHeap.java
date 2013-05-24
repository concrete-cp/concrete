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

package concrete.priorityqueues;

import java.util.Arrays;
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
public final class BinaryHeap<T extends Identified> implements PriorityQueue<T> {

    private T[] content;

    private int[] queuePosition;

    private int[] inQueue;

    private int[] keyValue;

    private int size;

    private int iter = 0;

    public BinaryHeap() {
        this(10);
    }

    @SuppressWarnings("unchecked")
    public BinaryHeap(final int initSize) {
        super();
        this.queuePosition = new int[initSize];
        this.inQueue = new int[initSize];
        this.content = (T[]) new Identified[initSize];
        this.keyValue = new int[initSize];
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
    private void ensureMapCapacity(final int minCapacity) {
        int oldCapacity = queuePosition.length;
        assert inQueue.length == oldCapacity;
        if (minCapacity > oldCapacity) {
            final int newCapacity = Math.max(minCapacity,
                    (oldCapacity * 3) / 2 + 1);
            // minCapacity is usually close to size, so this is a win:
            queuePosition = Arrays.copyOf(queuePosition, newCapacity);
            inQueue = Arrays.copyOf(inQueue, newCapacity);
            Arrays.fill(inQueue, oldCapacity, newCapacity, -1);
        }
    }

    private void ensureHeapCapacity(final int minCapacity) {
        int oldCapacity = content.length;
        if (minCapacity > oldCapacity) {
            final int newCapacity = Math.max(minCapacity,
                    (oldCapacity * 3) / 2 + 1);
            // minCapacity is usually close to size, so this is a win:
            content = Arrays.copyOf(content, newCapacity);
            keyValue = Arrays.copyOf(keyValue, newCapacity);
        }
    }

  

    @Override
    public boolean offer(final T arg0, final int eval) {
        final int id = arg0.getId();
        ensureMapCapacity(id + 1);

        if (inQueue[id] == iter) {
            final int position = queuePosition[id];
            keyValue[position] = eval;
            siftUp(position);
            if (position == queuePosition[id]) {
                siftDown(position);
            }
            return false;
        } else {
            ensureHeapCapacity(size + 1);
            content[size] = arg0;
            keyValue[size] = eval;
            queuePosition[id] = size;
            inQueue[id] = iter;
            siftUp(size++);
            return true;
        }
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
            size--;
            content[0] = content[size];
            keyValue[0] = keyValue[size];
            queuePosition[content[0].getId()] = 0;
            siftDown(0);
            inQueue[max.getId()] = -1;
            return max;
        }
    }

  

    @Override
    public void clear() {
        size = 0;
        iter++;
    }
    
    public boolean isEmpty() {
        return size == 0;
    }


    private void swap(final int int0, final int int1) {
        final T tmp = content[int0];
        content[int0] = content[int1];
        content[int1] = tmp;
        final int tmp2 = keyValue[int0];
        keyValue[int0] = keyValue[int1];
        keyValue[int1] = tmp2;
        queuePosition[content[int0].getId()] = int0;
        queuePosition[content[int1].getId()] = int1;
    }

    private void siftDown(final int start) {
        int root = start;
        final int end = size - 1;
        int child;
        while ((child = (root << 1) + 1) <= end) {
            if (child < end && keyValue[child + 1] < keyValue[child]) {
                child++;
            }
            if (keyValue[child] < keyValue[root]) {
                swap(root, child);
                root = child;
            } else {
                break;
            }
        }
    }

    private void siftUp(final int start) {
        int leaf = start;
        while (leaf > 0) {
            final int parent = ((leaf - 1) >> 1);

            if (keyValue[leaf] < keyValue[parent]) {
                swap(parent, leaf);
                leaf = parent;
            } else {
                break;
            }
        }
    }

    public String toString() {
        return Arrays.toString(content);
    }

//    private boolean control(final int position) {
//        if (position >= size) {
//            return true;
//        }
//        int child1 = (position << 1) + 1;
//        int child2 = (position << 1) + 2;
//        final int thisKey = key.getKey(content[position]);
//
//        if (child1 < size && thisKey > key.getKey(content[child1])) {
//            return false;
//        }
//        if (child2 < size && thisKey > key.getKey(content[child2])) {
//            return false;
//        }
//        return control(child1) && control(child2);
//
//    }
}
