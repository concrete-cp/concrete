/**
 * --------------------------
 * FibonnaciHeap.java
 * --------------------------
 * (C) Copyright 1999-2008, by Nathan Fiedler and Contributors.
 *
 * Original Author:  Nathan Fiedler
 */
package cspfj.util;

import java.util.AbstractQueue;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * This class implements a Fibonacci heap data structure. Much of the code in
 * this class is based on the algorithms in the "Introduction to Algorithms"by
 * Cormen, Leiserson, and Rivest in Chapter 21. The amortized running time of
 * most of these methods is O(1), making it a very fast data structure. Several
 * have an actual running time of O(1). removeMin() and delete() have O(log n)
 * amortized running times because they do the heap consolidation.
 * 
 * <p>
 * <b>Note that this implementation is not synchronized.</b> If multiple threads
 * access a set concurrently, and at least one of the threads modifies the set,
 * it <i>must</i> be synchronized externally. This is typically accomplished by
 * synchronizing on some object that naturally encapsulates the set.
 * </p>
 * 
 * <p>
 * This class was originally developed by Nathan Fiedler for the GraphMaker
 * project.
 * </p>
 * 
 * @author Nathan Fiedler
 */
public final class FibonacciHeap<T extends Identified> extends AbstractQueue<T> {
    /**
     * The magic 45 comes from log base phi of Integer.MAX_VALUE, which is the
     * most elements we will ever hold, and log base phi represents the largest
     * degree of any root list node.
     */
    private static final int MAX_ARRAY_SIZE = 45;

    private final FibonacciHeapNode<T>[] array = (FibonacciHeapNode<T>[]) new FibonacciHeapNode[MAX_ARRAY_SIZE];

    /**
     * Points to the minimum node in the heap.
     */
    private FibonacciHeapNode<T> minNode;

    /**
     * Number of nodes in the heap.
     */
    private int nNodes;

    private final FibonacciHeapNode<T>[] map;

    private final boolean[] inQueue;

    private final Key<T> key;

    /**
     * Constructs a FibonacciHeap object that contains no elements.
     */
    public FibonacciHeap(final Key<T> key, final T[] values) {
        this.key = key;
        map = (FibonacciHeapNode<T>[]) new FibonacciHeapNode<?>[values.length];
        for (T v : values) {
            map[v.getId()] = new FibonacciHeapNode<T>(v);
        }
        inQueue = new boolean[map.length];
    }

    /**
     * Tests if the Fibonacci heap is empty or not. Returns true if the heap is
     * empty, false otherwise.
     * 
     * <p>
     * Running time: O(1) actual
     * </p>
     * 
     * @return true if the heap is empty, false otherwise
     */
    public boolean isEmpty() {
        return minNode == null;
    }

    /**
     * Removes all elements from this heap.
     */
    public void clear() {
        minNode = null;
        nNodes = 0;
        Arrays.fill(inQueue, false);
    }

    /**
     * Decreases the key value for a heap node, given the new value to take on.
     * The structure of the heap may be changed and will not be consolidated.
     * 
     * <p>
     * Running time: O(1) amortized
     * </p>
     * 
     * @param x
     *            node to decrease the key of
     * 
     * @exception IllegalArgumentException
     *                Thrown if k is larger than x.key value.
     */
    private void decreaseKey(FibonacciHeapNode<T> x, boolean delete) {
        final FibonacciHeapNode<T> y = x.getParent();
        if (y != null && (delete || x.getKey() < y.getKey())) {
            y.cut(x, minNode);
            y.cascadingCut(minNode);
        }
        if (delete || x.getKey() < minNode.getKey()) {
            minNode = x;
        }

        assert control(minNode, minNode);
    }

    private void increaseKey(FibonacciHeapNode<T> x) {
        decreaseKey(x, true);
        removeMin();
        x.clear();
        insert(x);
    }

    /**
     * Inserts a new data element into the heap. No heap consolidation is
     * performed at this time, the new node is simply inserted into the root
     * list of this heap.
     * 
     * <p>
     * Running time: O(1) actual
     * </p>
     * 
     * @param node
     *            new node to insert into heap
     */
    private void insert(FibonacciHeapNode<T> node) {
        // concatenate node into min list
        if (minNode != null) {
            minNode.add(node);

            if (node.getKey() < minNode.getKey()) {
                minNode = node;
            }
        } else {
            minNode = node;
            node.setLeft(node);
            node.setRight(node);
        }

        nNodes++;
    }

    public boolean offer(T data) {
        final int id = data.getId();
        final FibonacciHeapNode<T> node = map[id];

        final int oldKey = node.getKey();
        final int newKey = key.getKey(data);
        node.setKey(newKey);

        if (inQueue[id]) {
            if (newKey < oldKey) {
                decreaseKey(node, false);
                assert smallest(minNode);
            } else if (newKey > oldKey) {
                increaseKey(node);
                assert smallest(minNode);
            }

            return false;
        }

        node.clear();
        insert(node);
        inQueue[id] = true;
        return true;
    }

    /**
     * Removes the smallest element from the heap. This will cause the trees in
     * the heap to be consolidated, if necessary.
     * 
     * <p>
     * Running time: O(log n) amortized
     * </p>
     * 
     * @return node with the smallest key
     */
    private T removeMin() {
        final FibonacciHeapNode<T> z = minNode;

        if (z == null) {
            return null;
        }

        final FibonacciHeapNode<T> zChild = z.getChild();

        if (zChild != null) {
            zChild.setParent(null);
            // for each child of z do...
            for (FibonacciHeapNode<T> x = zChild.getRight(); x != zChild; x = x
                    .getRight()) {
                // set parent[x] to null
                x.setParent(null);
            }
            // merge the children into root list

            final FibonacciHeapNode<T> minleft = minNode.getLeft();
            final FibonacciHeapNode<T> zchildleft = zChild.getLeft();
            minNode.setLeft(zchildleft);
            zchildleft.setRight(minNode);
            zChild.setLeft(minleft);
            minleft.setRight(z.getChild());
        }
        // remove z from root list of heap
        z.remove();
        if (z == z.getRight()) {
            minNode = null;
        } else {
            minNode = z.getRight();
            consolidate();
        }
        // decrement size of heap
        nNodes--;

        assert control(minNode, minNode);
        return z.getData();
    }

    private boolean smallest(FibonacciHeapNode<T> min) {

        for (int i = inQueue.length; --i >= 0;) {
            if (inQueue[i] && map[i].getKey() < min.getKey()) {
                return false;
            }
        }

        return true;
    }

    /**
     * Returns the size of the heap which is measured in the number of elements
     * contained in the heap.
     * 
     * <p>
     * Running time: O(1) actual
     * </p>
     * 
     * @return number of elements in the heap
     */
    public int size() {
        return nNodes;
    }

    /**
     * Consolidates the trees in the heap by joining trees of equal degree until
     * there are no more trees of equal degree in the root list.
     * 
     * <p>
     * <em>Running time: O(log n) amortized</em>
     * </p>
     */
    private void consolidate() {
        Arrays.fill(array, null);

        // For each root list node look for others of the same degree.
        FibonacciHeapNode<T> start = minNode;
        FibonacciHeapNode<T> w = minNode;
        do {
            FibonacciHeapNode<T> x = w;
            // Because x might be moved, save its sibling now.
            FibonacciHeapNode<T> nextW = w.getRight();
            int d = x.getDegree();
            while (array[d] != null) {
                // Make one of the nodes a child of the other.
                FibonacciHeapNode<T> y = array[d];
                if (x.getKey() > y.getKey()) {
                    final FibonacciHeapNode<T> temp = y;
                    y = x;
                    x = temp;
                }
                if (y == start) {
                    // Because removeMin() arbitrarily assigned the min
                    // reference, we have to ensure we do not miss the
                    // end of the root node list.
                    start = start.getRight();
                }
                if (y == nextW) {
                    // If we wrapped around we need to check for this case.
                    nextW = nextW.getRight();
                }
                // Node y disappears from root list.
                y.link(x);
                // We've handled this degree, go to next one.
                array[d] = null;
                d++;
            }
            // Save this node for later when we might encounter another
            // of the same degree.
            array[d] = x;
            // Move forward through list.
            w = nextW;
        } while (w != start);

        // The node considered to be min may have been changed above.
        minNode = start;
        // Find the minimum key again.
        for (FibonacciHeapNode<T> a : array) {
            if (a != null && a.getKey() < minNode.getKey()) {
                minNode = a;
            }
        }

        assert control(minNode, minNode);
    }

    @Override
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    @Override
    public T peek() {
        assert smallest(minNode);
        return minNode.getData();
    }

    @Override
    public T poll() {
        assert smallest(minNode);
        final T min = removeMin();
        inQueue[min.getId()] = false;
        return min;
    }

    /**
     * Creates a String representation of this Fibonacci heap.
     * 
     * @return String of this.
     */
    public String toString() {
        if (minNode == null) {
            return "empty";
        }
        return tree(minNode, minNode, 0);
    }

    private boolean control(FibonacciHeapNode<T> current,
            FibonacciHeapNode<T> start) {
        return control(current, start, new HashSet<FibonacciHeapNode<T>>(),
                new HashSet<FibonacciHeapNode<T>>());
    }

    private boolean control(FibonacciHeapNode<T> current,
            FibonacciHeapNode<T> start, Set<FibonacciHeapNode<T>> loopControl,
            Set<FibonacciHeapNode<T>> ancestorControl) {
        if (current == null) {
            return true;
        }
        assert !loopControl.contains(current);
        assert !ancestorControl.contains(current);
        loopControl.add(current);
        if (current.getChild() != null) {
            ancestorControl.add(current);
            if (!control(current.getChild(), current.getChild(),
                    new HashSet<FibonacciHeapNode<T>>(), ancestorControl)) {
                return false;
            }
        }

        return current.getRight() == start
                || control(current.getRight(), start, loopControl,
                        ancestorControl);
    }

    private String tree(FibonacciHeapNode<T> current,
            FibonacciHeapNode<T> start, int depth) {
        final StringBuilder stb = new StringBuilder();
        for (int i = depth; --i >= 0;) {
            stb.append("--");
        }
        stb.append(current.getData()).append("\n");
        if (current.getChild() != null) {
            stb.append(tree(current.getChild(), current.getChild(), depth + 1));
        }
        if (current.getRight() != start) {
            stb.append(tree(current.getRight(), start, depth));
        }
        return stb.toString();
    }
}
