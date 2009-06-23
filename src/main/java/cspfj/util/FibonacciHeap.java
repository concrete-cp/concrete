/* ==========================================
 * JGraphT : a free Java graph-theory library
 * ==========================================
 *
 * Project Info:  http://jgrapht.sourceforge.net/
 * Project Creator:  Barak Naveh (barak_naveh@users.sourceforge.net)
 *
 * (C) Copyright 2003-2007, by Barak Naveh and Contributors.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 */
/* --------------------------
 * FibonnaciHeap.java
 * --------------------------
 * (C) Copyright 1999-2003, by Nathan Fiedler and Contributors.
 *
 * Original Author:  Nathan Fiedler
 * Contributor(s):   John V. Sichi
 *
 * $Id: FibonacciHeap.java 603 2008-06-28 07:51:50Z perfecthash $
 *
 * Changes
 * -------
 * 03-Sept-2003 : Adapted from Nathan Fiedler (JVS);
 *
 *      Name    Date            Description
 *      ----    ----            -----------
 *      nf      08/31/97        Initial version
 *      nf      09/07/97        Removed FibHeapData interface
 *      nf      01/20/01        Added synchronization
 *      nf      01/21/01        Made Node an inner class
 *      nf      01/05/02        Added clear(), renamed empty() to
 *                              isEmpty(), and renamed printHeap()
 *                              to toString()
 *      nf      01/06/02        Removed all synchronization
 *
 */
package cspfj.util;

import java.util.AbstractQueue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * This class implements a Fibonacci heap data structure. Much of the code in
 * this class is based on the algorithms in the "Introduction to Algorithms"by
 * Cormen, Leiserson, and Rivest in Chapter 21. The amortized running time of
 * most of these methods is O(1), making it a very fast data structure. Several
 * have an actual running time of O(1). removeMin() and delete() have O(log n)
 * amortized running times because they do the heap consolidation. If you
 * attempt to store nodes in this heap with key values of -Infinity
 * (Double.NEGATIVE_INFINITY) the <code>delete()</code> operation may fail to
 * remove the correct element.
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
 * project. It was imported to JGraphT with permission, courtesy of Nathan
 * Fiedler.
 * </p>
 * 
 * @author Nathan Fiedler
 */
public class FibonacciHeap<T extends Identified> extends AbstractQueue<T> {
    // ~ Static fields/initializers
    // ---------------------------------------------

    private static final double ONE_OVER_LOG_PHI = 1.0 / Math.log((1.0 + Math
            .sqrt(5.0)) / 2.0);

    // ~ Instance fields
    // --------------------------------------------------------

    /**
     * Points to the minimum node in the heap.
     */
    private FibonacciHeapNode<T> minNode;

    /**
     * Number of nodes in the heap.
     */
    private int nNodes;

    private final Comparator<T> comparator;

    private final FibonacciHeapNode<T>[] map;

    private final boolean[] inQueue;

    /**
     * Constructs a FibonacciHeap object that contains no elements.
     */
    public FibonacciHeap(final Comparator<T> comparator, final T[] values) {
        this.comparator = comparator;
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
     * @param k
     *            new key value for node x
     * 
     * @exception IllegalArgumentException
     *                Thrown if k is larger than x.key value.
     */
    private void decreaseKey(FibonacciHeapNode<T> x) {
        FibonacciHeapNode<T> y = x.getParent();

        if ((y != null) && (comparator.compare(x.getData(), y.getData()) < 0)) {
            cut(x, y);
            cascadingCut(y);
        }

        if (comparator.compare(x.getData(), minNode.getData()) < 0) {
            minNode = x;
        }
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
     * @param key
     *            key value associated with data object
     */
    private void insert(FibonacciHeapNode<T> node) {
        // concatenate node into min list
        if (minNode != null) {
            node.setLeft(minNode);
            node.setRight(minNode.getRight());
            minNode.setRight(node);
            node.getRight().setLeft(node);

            if (comparator.compare(node.getData(), minNode.getData()) < 0) {
                minNode = node;
            }
        } else {
            minNode = node;
        }

        nNodes++;
    }

    public boolean offer(T data) {
        final int id = data.getId();
        map[id].clear();
        if (inQueue[id]) {
            decreaseKey(map[id]);
            return false;
        }
        insert(map[id]);
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
    private FibonacciHeapNode<T> removeMin() {
        final FibonacciHeapNode<T> z = minNode;

        if (z != null) {
            assert realSmallest(minNode);

            int numKids = z.getDegree();
            FibonacciHeapNode<T> x = z.getChild();
            FibonacciHeapNode<T> tempRight;

            // for each child of z do...
            while (numKids > 0) {
                tempRight = x.getRight();

                // remove x from child list
                x.getLeft().setRight(x.getRight());
                x.getRight().setLeft(x.getLeft());

                // add x to root list of heap
                x.setLeft(minNode);
                x.setRight(minNode.getRight());
                minNode.setRight(x);
                x.getRight().setLeft(x);

                // set parent[x] to null
                x.setParent(null);
                x = tempRight;
                numKids--;
            }

            // remove z from root list of heap
            z.getLeft().setRight(z.getRight());
            z.getRight().setLeft(z.getLeft());

            if (z == z.getRight()) {
                minNode = null;
            } else {
                minNode = z.getRight();
                consolidate();
            }

            // decrement size of heap
            nNodes--;
        }

        return z;
    }

    private boolean realSmallest(FibonacciHeapNode<T> min) {

        for (int i = inQueue.length; --i >= 0;) {
            if (inQueue[i]
                    && comparator.compare(map[i].getData(), min.getData()) < 0) {
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
     * Creates a String representation of this Fibonacci heap.
     * 
     * @return String of this.
     */
    public String toString() {
        if (minNode == null) {
            return "FibonacciHeap=[]";
        }

        // create a new stack and put root on it
        final Deque<FibonacciHeapNode<T>> stack = new LinkedList<FibonacciHeapNode<T>>();
        stack.push(minNode);

        final StringBuilder buf = new StringBuilder(512);
        buf.append("FibonacciHeap=[");

        // do a simple breadth-first traversal on the tree
        while (!stack.isEmpty()) {
            FibonacciHeapNode<T> curr = stack.pop();
            buf.append(curr);
            buf.append(", ");

            if (curr.getChild() != null) {
                stack.push(curr.getChild());
            }

            final FibonacciHeapNode<T> start = curr;
            curr = curr.getRight();

            while (curr != start) {
                buf.append(curr);
                buf.append(", ");

                if (curr.getChild() != null) {
                    stack.push(curr.getChild());
                }

                curr = curr.getRight();
            }
        }

        buf.append(']');

        return buf.toString();
    }

    /**
     * Performs a cascading cut operation. This cuts y from its parent and then
     * does the same for its parent, and so on up the tree.
     * 
     * <p>
     * Running time: O(log n); O(1) excluding the recursion
     * </p>
     * 
     * @param y
     *            node to perform cascading cut on
     */
    private void cascadingCut(FibonacciHeapNode<T> y) {
        final FibonacciHeapNode<T> z = y.getParent();

        // if there's a parent...
        if (z != null) {
            // if y is marked, cut it from parent
            if (y.isMark()) {
                cut(y, z);

                // cut its parent as well
                cascadingCut(z);
            } else {
                // it's unmarked, set it marked
                y.setMark(true);
            }
        }
    }

    private void consolidate() {
        final int arraySize = ((int) Math.floor(Math.log(nNodes)
                * ONE_OVER_LOG_PHI)) + 1;

        final List<FibonacciHeapNode<T>> array = new ArrayList<FibonacciHeapNode<T>>(
                arraySize);

        // Initialize degree array
        for (int i = 0; i < arraySize; i++) {
            array.add(null);
        }

        // Find the number of root nodes.
        int numRoots = 0;
        FibonacciHeapNode<T> x = minNode;

        if (x != null) {
            numRoots++;
            x = x.getRight();

            while (x != minNode) {
                numRoots++;
                x = x.getRight();
            }
        }

        // For each node in root list do...
        while (numRoots > 0) {
            // Access this node's degree..
            int d = x.getDegree();
            final FibonacciHeapNode<T> next = x.getRight();

            // ..and see if there's another of the same degree.
            for (;;) {
                FibonacciHeapNode<T> y = array.get(d);
                if (y == null) {
                    // Nope.
                    break;
                }

                // There is, make one of the nodes a child of the other.
                // Do this based on the key value.
                if (comparator.compare(x.getData(), y.getData()) > 0) {
                    final FibonacciHeapNode<T> temp = y;
                    y = x;
                    x = temp;
                }

                // FibonacciHeapNode<T> y disappears from root list.
                link(y, x);

                // We've handled this degree, go to next one.
                array.set(d, null);
                d++;
            }

            // Save this node for later when we might encounter another
            // of the same degree.
            array.set(d, x);

            // Move forward through list.
            x = next;
            numRoots--;
        }

        // Set min to null (effectively losing the root list) and
        // reconstruct the root list from the array entries in array[].
        minNode = null;

        for (int i = 0; i < arraySize; i++) {
            final FibonacciHeapNode<T> y = array.get(i);
            if (y == null) {
                continue;
            }

            // We've got a live one, add it to root list.
            if (minNode == null) {
                minNode = y;
            } else {
                // First remove node from root list.
                y.getLeft().setRight(y.getRight());
                y.getRight().setLeft(y.getLeft());

                // Now add to root list, again.
                y.setLeft(minNode);
                y.setRight(minNode.getRight());
                minNode.setRight(y);
                y.getRight().setLeft(y);

                // Check if this is a new min.
                if (comparator.compare(y.getData(), minNode.getData()) < 0) {
                    minNode = y;
                }
            }
        }
    }

    /**
     * The reverse of the link operation: removes x from the child list of y.
     * This method assumes that min is non-null.
     * 
     * <p>
     * Running time: O(1)
     * </p>
     * 
     * @param x
     *            child of y to be removed from y's child list
     * @param y
     *            parent of x about to lose a child
     */
    private void cut(FibonacciHeapNode<T> x, FibonacciHeapNode<T> y) {
        // remove x from childlist of y and decrement degree[y]
        x.getLeft().setRight(x.getRight());
        x.getRight().setLeft(x.getLeft());
        y.setDegree(y.getDegree() - 1);

        // reset y.child if necessary
        if (y.getChild() == x) {
            y.setChild(x.getRight());
        }

        if (y.getDegree() == 0) {
            y.setChild(null);
        }

        // add x to root list of heap
        x.setLeft(minNode);
        x.setRight(minNode.getRight());
        minNode.setRight(x);
        x.getRight().setLeft(x);

        // set parent[x] to nil
        x.setParent(null);

        // set mark[x] to false
        x.setMark(false);
    }

    /**
     * Make node y a child of node x.
     * 
     * <p>
     * Running time: O(1) actual
     * </p>
     * 
     * @param y
     *            node to become child
     * @param x
     *            node to become parent
     */
    private void link(FibonacciHeapNode<T> y, FibonacciHeapNode<T> x) {
        // remove y from root list of heap
        y.getLeft().setRight(y.getRight());
        y.getRight().setLeft(y.getLeft());

        // make y a child of x
        y.setParent(x);

        if (x.getChild() == null) {
            x.setChild(y);
            y.setRight(y);
            y.setLeft(y);
        } else {
            y.setLeft(x.getChild());
            y.setRight(x.getChild().getRight());
            x.getChild().setRight(y);
            y.getRight().setLeft(y);
        }

        // increase degree[x]
        x.setDegree(x.getDegree() + 1);

        // set mark[y] false
        y.setMark(false);
    }

    @Override
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    @Override
    public T peek() {
        return minNode.getData();
    }

    @Override
    public T poll() {
        final T min = removeMin().getData();
        inQueue[min.getId()] = false;
        return min;
    }

}

// FibonacciHeap
