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

import java.util.AbstractSet;
import java.util.Iterator;
import java.util.NoSuchElementException;

public final class Maximier<T extends Comparable<T>> extends AbstractSet<T> {

    private final T[] content;

    private int size;

    private int firstAbsent;

    public Maximier(final T[] arg0) {
        super();
        content = arg0;

        this.clear();
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean add(final T arg0) {
        if (size >= content.length) {
            return false;
        }

        // System.out.println("adding " + arg0);
        content[firstAbsent] = arg0;
        size++;

        if (size >= content.length) {
            firstAbsent = -1;
        } else {
            while (content[firstAbsent] != null) {
                firstAbsent++;
            }
        }

        return true;
    }

    public void sort() {
        sort(0);
    }

    private void sort(final int index) {
        if (leftChild(index) != null) {
            sort(leftChildIndex(index));
        }
        if (rightChild(index) != null) {
            sort(rightChildIndex(index));
        }
        reorganize(index);
    }

    private void reorganize(final int index) {
        int max = index;
        if (leftChild(index) != null
                && content[index].compareTo(leftChild(index)) > 0) {
            max = leftChildIndex(index);
        }
        if (rightChild(index) != null
                && content[max].compareTo(rightChild(index)) > 0) {
            max = rightChildIndex(index);
        }

        if (max != index) {
            final T cont = content[index];
            content[index] = content[max];
            content[max] = cont;
            reorganize(max);
        }

    }

    public T pull() throws NoSuchElementException {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }

        final T max = content[0];
        delete(0);
        size--;
        return max;
    }

    private T delete(final int index) {
        final T cont = content[index];
        if (rightChild(index) == null && leftChild(index) == null) {
            content[index] = null;
            if (index < firstAbsent || firstAbsent < 0) {
                firstAbsent = index;
            }

        } else if (rightChild(index) == null) {
            content[index] = delete(leftChildIndex(index));
        } else if (leftChild(index) == null) {
            content[index] = delete(rightChildIndex(index));
        } else if (leftChild(index).compareTo(rightChild(index)) < 0) {
            content[index] = delete(leftChildIndex(index));
        } else {
            content[index] = delete(rightChildIndex(index));
        }
        return cont;

    }

    private static int leftChildIndex(final int index) {
        return index * 2 + 1;
    }

    private static int rightChildIndex(final int index) {
        return index * 2 + 2;
    }

    private T leftChild(final int index) {
        if (leftChildIndex(index) >= content.length) {
            return null;
        }
        return content[leftChildIndex(index)];
    }

    private T rightChild(final int index) {
        if (rightChildIndex(index) >= content.length) {
            return null;
        }
        return content[rightChildIndex(index)];

    }

    @Override
    public void clear() {
        for (int i = 0; i < content.length; i++) {
            content[i] = null;
        }
        firstAbsent = 0;
        size = 0;
    }

    @Override
    public Iterator<T> iterator() {
        // TODO Auto-generated method stub
        return null;
    }

    public boolean contains(final T arg0) {
        if (isEmpty()) {
            return false;
        }
        return contains(arg0, 0);
    }

    private boolean contains(final T arg0, final int index) {
        if (content[index].compareTo(arg0) == 0) {
            return true;
        }
        if (content[index].compareTo(arg0) < 0) {
            return false;
        }
        if (leftChild(index) != null && contains(arg0, leftChildIndex(index))) {
            return true;
        }
        if (rightChild(index) != null && contains(arg0, rightChildIndex(index))) {
            return true;
        }
        return false;
    }

    // public static void main(final String[] args) {
    // final Maximier<Integer> m = new Maximier<Integer>(new Integer[1000000],
    // new C());
    //
    // Random r = new Random();
    //
    // for (int i = 0; i < 1000000; i++) {
    // m.add(r.nextInt(100));
    // }
    //
    // //System.out.println(m);
    //
    // m.sort();
    // //System.out.println(m);
    // // System.out.println(C.getOps());
    //        
    //
    // // while (!m.isEmpty()) {
    // // System.out.print(m.pull() + " ");
    // // // System.out.println(m);
    // // }
    // // System.out.println();
    //
    // }
    //
    // public static class C implements Comparator<Integer> {
    // private static int ops = 0;
    //
    // public int compare(Integer arg0, Integer arg1) {
    // // TODO Auto-generated method stub
    // ops++;
    // return arg0.compareTo(arg1);
    // }
    //
    // public static int getOps() {
    // return ops;
    // }
    //
    // }

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        for (int i = 0; i < content.length; i++) {
            // if (content[i] == null) {
            // sb.append("null");
            // } else {
            sb.append(content[i]);
            // }
            sb.append(' ');
        }
        return sb.toString();
    }
}
