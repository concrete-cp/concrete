/**
 * --------------------------
 * FibonnaciHeap.java
 * --------------------------
 * (C) Copyright 1999-20089, by Julien Vion, Nathan Fiedler and Contributors.
 *
 * Original Author:  Nathan Fiedler
 */
package cspfj.priorityqueues;

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

    private static final int DEFAULT_SIZE = 10;

    private final FibonacciHeapNode<T>[] array;

    /**
     * Points to the minimum node in the heap.
     */
    private FibonacciHeapNode<T> minNode;

    /**
     * Number of nodes in the heap.
     */
    private int nNodes;

    private FibonacciHeapNode<T>[] map;

    private final Key<T> key;

    public static int insert = 0;
    public static int update = 0;
    public static int remove = 0;

    private int iter = 0;

    public FibonacciHeap(final Key<T> key) {
        this(key, DEFAULT_SIZE);
    }

    /**
     * Constructs a FibonacciHeap object that contains no elements.
     */
    @SuppressWarnings("unchecked")
    public FibonacciHeap(final Key<T> key, final int initSize) {
        this.key = key;
        map = (FibonacciHeapNode<T>[]) new FibonacciHeapNode<?>[initSize];
        array = (FibonacciHeapNode<T>[]) new FibonacciHeapNode[MAX_ARRAY_SIZE];

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
        iter++;
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
        int oldCapacity = map.length;
        if (minCapacity > oldCapacity) {
            final int newCapacity = Math.max(minCapacity,
                    (oldCapacity * 3) / 2 + 1);
            // minCapacity is usually close to size, so this is a win:
            map = Arrays.copyOf(map, newCapacity);
        }
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
     * @param delete
     *            set to true if the node is going to be deleted: it will be
     *            sifted up to the root (as if the new key was negative
     *            infinity) for easy deletion
     * @exception IllegalArgumentException
     *                Thrown if k is larger than x.key value.
     */
    private void decreaseKey(final FibonacciHeapNode<T> x, final boolean delete) {
        final FibonacciHeapNode<T> y = x.parent;
        if (y != null && (delete || x.key < y.key)) {
            y.cut(x, minNode);
            y.cascadingCut(minNode);
        }
        if (delete || x.key < minNode.key) {
            minNode = x;
        }

        //assert control(minNode, minNode);
    }

    /**
     * Key increase is implemented as deletion and reinsertion of the node.
     * 
     * @param x
     */
    private void increaseKey(final FibonacciHeapNode<T> x) {
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
    private void insert(final FibonacciHeapNode<T> node) {
        // concatenate node into min list
        if (minNode != null) {
            minNode.add(node);

            if (node.key < minNode.key) {
                minNode = node;
            }
        } else {
            minNode = node;
            node.left = node;
            node.right = node;
        }

        nNodes++;
    }

    @Override
    public boolean offer(final T data) {
        final int id = data.getId();

        ensureCapacity(id + 1);

        FibonacciHeapNode<T> node = map[id];
        if (node == null) {
            node = new FibonacciHeapNode<T>(data);
            map[id] = node;
        }

        final double oldKey = node.key;
        final double newKey = key.getKey(data);
        node.key = newKey;

        if (node.inQueue == iter) {
            update++;
            if (newKey < oldKey) {
                decreaseKey(node, false);
                assert smallest(minNode);
            } else if (newKey > oldKey) {
                increaseKey(node);
                assert smallest(minNode);
            }

            return false;
        }
        insert++;
        node.clear();
        insert(node);
        node.inQueue = iter;
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

        if (z == null) {
            return null;
        }

        final FibonacciHeapNode<T> zChild = z.child;

        if (zChild != null) {
            zChild.parent = null;
            // for each child of z do...
            for (FibonacciHeapNode<T> x = zChild.right; x != zChild; x = x.right) {
                // set parent[x] to null
                x.parent = null;
            }
            // merge the children into root list

            final FibonacciHeapNode<T> minLeft = minNode.left;
            final FibonacciHeapNode<T> zChildLeft = zChild.left;
            minNode.left = zChildLeft;
            zChildLeft.right = minNode;
            zChild.left = minLeft;
            minLeft.right = z.child;
        }
        // remove z from root list of heap
        z.remove();
        if (z == z.right) {
            minNode = null;
        } else {
            minNode = z.right;
            consolidate();
        }
        // decrement size of heap
        nNodes--;

        //assert control(minNode, minNode);
        return z;
    }

    private boolean smallest(final FibonacciHeapNode<T> min) {

        for (FibonacciHeapNode<T> n : map) {
            if (n != null && n.inQueue == iter && n.key < min.key) {
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
            FibonacciHeapNode<T> nextW = w.right;
            int d = x.degree;
            while (array[d] != null) {
                // Make one of the nodes a child of the other.
                FibonacciHeapNode<T> y = array[d];
                if (x.key > y.key) {
                    final FibonacciHeapNode<T> temp = y;
                    y = x;
                    x = temp;
                }
                if (y == start) {
                    /*
                     * Because removeMin() arbitrarily assigned the min
                     * reference, we have to ensure we do not miss the end of
                     * the root node list.
                     */
                    start = start.right;
                }
                if (y == nextW) {
                    // If we wrapped around we need to check for this case.
                    nextW = nextW.right;
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
            if (a != null && a.key < minNode.key) {
                minNode = a;
            }
        }

        //assert control(minNode, minNode);
    }

    @Override
    public Iterator<T> iterator() {
        throw new UnsupportedOperationException();
    }

    @Override
    public T peek() {
        assert smallest(minNode);
        return minNode.data;
    }

    @Override
    public T poll() {
        assert smallest(minNode);
        final FibonacciHeapNode<T> min = removeMin();
        min.inQueue = -1;
        return min.data;
    }

//    private boolean control(final FibonacciHeapNode<T> current,
//            final FibonacciHeapNode<T> start) {
//        return control(current, start, new HashSet<FibonacciHeapNode<T>>(),
//                new HashSet<FibonacciHeapNode<T>>());
//    }
//
//    private boolean control(final FibonacciHeapNode<T> current,
//            final FibonacciHeapNode<T> start,
//            final Set<FibonacciHeapNode<T>> loopControl,
//            final Set<FibonacciHeapNode<T>> ancestorControl) {
//        if (current == null) {
//            return true;
//        }
//        assert !loopControl.contains(current);
//        assert !ancestorControl.contains(current);
//        loopControl.add(current);
//        if (current.child != null) {
//            ancestorControl.add(current);
//            if (!control(current.child, current.child,
//                    new HashSet<FibonacciHeapNode<T>>(), ancestorControl)) {
//                return false;
//            }
//        }
//
//        return current.right == start
//                || control(current.right, start, loopControl, ancestorControl);
//    }

    /**
     * Creates a String representation of this Fibonacci heap.
     * 
     * @return String of this.
     */
    public String toString() {
        if (minNode == null) {
            return "empty";
        }
        final StringBuilder stb = new StringBuilder();
        tree(stb, minNode, minNode, 0);
        return stb.toString();
    }

    private static <T> void tree(final StringBuilder stb,
            final FibonacciHeapNode<T> current,
            final FibonacciHeapNode<T> start, final int depth) {

        for (int i = depth; --i >= 0;) {
            stb.append("--");
        }
        stb.append(current.data).append("\n");
        if (current.child != null) {
            tree(stb, current.child, current.child, depth + 1);
        }
        if (current.right != start) {
            tree(stb, current.right, start, depth);
        }
    }

    /**
     * Implements a node of the Fibonacci heap. It holds the information
     * necessary for maintaining the structure of the heap. It also holds the
     * reference to the key value (which is used to determine the heap
     * structure).
     * 
     * @author Nathan Fiedler
     */
    private static final class FibonacciHeapNode<T> {
        /**
         * Node data.
         */
        private final T data;

        /**
         * first child node.
         */
        private FibonacciHeapNode<T> child;

        /**
         * left sibling node.
         */
        private FibonacciHeapNode<T> left;

        /**
         * parent node.
         */
        private FibonacciHeapNode<T> parent;

        /**
         * right sibling node.
         */
        private FibonacciHeapNode<T> right;

        /**
         * true if this node has had a child removed since this node was added
         * to its parent.
         */
        private boolean mark;

        /**
         * number of children of this node (does not count grandchildren).
         */
        private int degree;

        private double key;

        private int inQueue = -1;

        /**
         * Default constructor. Initializes the right and left pointers, making
         * this a circular doubly-linked list.
         * 
         * @param data
         *            data for this node
         * @param key
         *            initial key for node
         */
        private FibonacciHeapNode(final T data) {
            this.data = data;
            right = this;
            left = this;
            clear();
        }

        /**
         * Initializes parents and child information.
         */
        public void clear() {
            mark = false;
            degree = 0;
            // setRight(this);
            // setLeft(this);
            child = null;
            parent = null;
        }

        /**
         * Remove this node from the list it appears in.
         */
        public void remove() {
            left.right = right;
            right.left = left;
        }

        /**
         * Adds x to the left of this node in the list.
         * 
         * @param x
         */
        public void add(final FibonacciHeapNode<T> x) {
            x.right = this;
            x.left = left;
            left = x;
            x.left.right = x;
        }

        /**
         * Make this node a child of the given parent node. All linkages are
         * updated, the degree of the parent is incremented, and mark is set to
         * false.
         * 
         * @param parent
         *            the new parent node.
         */
        public void link(final FibonacciHeapNode<T> parent) {
            /**
             * Note: putting this code here in Node makes it 7x faster because
             * it doesn't have to use generated accessor methods, which add a
             * lot of time when called millions of times.
             */

            // Remove this from its circular list
            left.right = right;
            right.left = left;
            // make this a child of x
            this.parent = parent;
            if (parent.child == null) {
                parent.child = this;
                right = this;
                left = this;
            } else {
                left = parent.child;
                right = parent.child.right;
                parent.child.right = this;
                right.left = this;
            }
            // increase degree[x]
            parent.degree++;
            // set mark false
            mark = false;
        }

        /**
         * Performs a cascading cut operation. Cuts this from its parent and
         * then does the same for its parent, and so on up the tree.
         * 
         * <p>
         * <em>Running time: O(log n)</em>
         * </p>
         * 
         * @param min
         *            the minimum heap node, to which nodes will be added.
         */
        public void cascadingCut(final FibonacciHeapNode<T> min) {
            FibonacciHeapNode<T> z = parent;
            // if there's a parent...
            if (z != null) {
                if (mark) {
                    // it's marked, cut it from parent
                    z.cut(this, min);
                    // cut its parent as well
                    z.cascadingCut(min);
                } else {
                    // if y is unmarked, set it marked
                    mark = true;
                }
            }
        }

        /**
         * The reverse of the link operation: removes x from the child list of
         * this node.
         * 
         * <p>
         * <em>Running time: O(1)</em>
         * </p>
         * 
         * @param x
         *            child to be removed from this node's child list
         * @param min
         *            the minimum heap node, to which x is added.
         */
        public void cut(final FibonacciHeapNode<T> x,
                final FibonacciHeapNode<T> min) {
            // remove x from childlist and decrement degree
            x.left.right = x.right;
            x.right.left = x.left;
            degree--;
            // reset child if necessary
            if (degree == 0) {
                child = null;
            } else if (child == x) {
                child = x.right;
            }
            // add x to root list of heap
            x.right = min;
            x.left = min.left;
            min.left = x;
            x.left.right = x;
            // set parent[x] to nil
            x.parent = null;
            // set mark[x] to false
            x.mark = false;
        }

        public String toString() {

            final StringBuilder buf = new StringBuilder();
            buf.append("Node=[");
            if (left != null) {
                buf.append(left.data);
            } else {
                buf.append(" * ");
            }
            buf.append(" <- ").append(data).append(" -> ");
            if (right != null) {
                buf.append(right.data);
            } else {
                buf.append(" * ");
            }

            buf.append(", ^: ");

            if (parent != null) {
                buf.append(parent.data);
            } else {
                buf.append(" * ");
            }

            buf.append(", v: ");

            if (child != null) {
                buf.append(child.data);
            } else {
                buf.append(" * ");
            }

            buf.append(" (").append(degree).append(")]");

            return buf.toString();

        }
    }
}
