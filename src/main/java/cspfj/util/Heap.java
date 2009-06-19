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

package cspfj.util;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;

public final class Heap<T extends Identified> extends AbstractQueue<T> {

    private final T[] content;

    private final boolean[] inQueue;

    private int size;

    private final Comparator<T> comparator;

    public Heap(final Comparator<T> comparator, final T[] values) {
        super();
        this.comparator = comparator;
        this.inQueue = new boolean[values.length];
        this.content = values.clone();
        Arrays.fill(this.content, null);
        size = 0;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean offer(final T arg0) {
        if (inQueue[arg0.getId()]) {
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
                    && comparator.compare(content[child], content[child + 1]) > 0) {
                child++;
            }
            if (comparator.compare(content[root], content[child]) > 0) {
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

            if (comparator.compare(content[parent], content[leaf]) > 0) {
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
