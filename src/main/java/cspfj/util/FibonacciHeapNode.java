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
 * FibonnaciHeapNode.java
 * --------------------------
 * (C) Copyright 1999-2007, by Nathan Fiedler and Contributors.
 *
 * Original Author:  Nathan Fiedler
 * Contributor(s):   John V. Sichi
 *
 * $Id: FibonacciHeapNode.java 568 2007-09-30 00:12:18Z perfecthash $
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
 *      JVS     06/24/06        Generics
 *
 */
package cspfj.util;

/**
 * Implements a node of the Fibonacci heap. It holds the information necessary
 * for maintaining the structure of the heap. It also holds the reference to the
 * key value (which is used to determine the heap structure).
 * 
 * @author Nathan Fiedler
 */
public final class FibonacciHeapNode<T> {
    // ~ Instance fields
    // --------------------------------------------------------

    /**
     * Node data.
     */
    private final T data;

    /**
     * first child node
     */
    private FibonacciHeapNode<T> child;

    /**
     * left sibling node
     */
    private FibonacciHeapNode<T> left;

    /**
     * parent node
     */
    private FibonacciHeapNode<T> parent;

    /**
     * right sibling node
     */
    private FibonacciHeapNode<T> right;

    /**
     * true if this node has had a child removed since this node was added to
     * its parent
     */
    private boolean mark;

    /**
     * number of children of this node (does not count grandchildren)
     */
    private int degree;

    // ~ Constructors
    // -----------------------------------------------------------

    /**
     * Default constructor. Initializes the right and left pointers, making this
     * a circular doubly-linked list.
     * 
     * @param data
     *            data for this node
     * @param key
     *            initial key for node
     */
    public FibonacciHeapNode(T data) {
        this.data = data;
        setRight(this);
        setLeft(this);
        clear();
    }

    // ~ Methods
    // ----------------------------------------------------------------
    
    /**
     * Initializes parents and child information
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
     * Obtain the data for this node.
     */
    public final T getData() {
        return data;
    }

    /**
     * Sets the parent node
     * 
     * @param parent
     */
    public void setParent(FibonacciHeapNode<T> parent) {
        this.parent = parent;
    }

    /**
     * @return the parent node
     */
    public FibonacciHeapNode<T> getParent() {
        return parent;
    }

    public String toString() {

        StringBuilder buf = new StringBuilder();
        buf.append("Node=[");
        if (getLeft() != null) {
            buf.append(getLeft().data);
        } else {
            buf.append(" * ");
        }
        buf.append(" <- ").append(data).append(" -> ");
        if (getRight() != null) {
            buf.append(getRight().data);
        } else {
            buf.append(" * ");
        }

        buf.append(", ^: ");

        if (getParent() != null) {
            buf.append(getParent().data);
        } else {
            buf.append(" * ");
        }

        buf.append(", v: ");

        if (getChild() != null) {
            buf.append(getChild().data);
        } else {
            buf.append(" * ");
        }

        buf.append(" (").append(degree).append(")]");

        return buf.toString();

    }

    /**
     * Sets the given node as the left neighbour of this node in the list
     * 
     * @param right
     */
    public void setLeft(FibonacciHeapNode<T> left) {
        this.left = left;
    }

    /**
     * Returns the node to the left of this node
     * 
     * @return
     */
    public FibonacciHeapNode<T> getLeft() {
        return left;
    }

    /**
     * Sets the given node as the right neighbour of this node in the list
     * 
     * @param right
     */
    public void setRight(FibonacciHeapNode<T> right) {
        this.right = right;
    }

    /**
     * Returns the node to the right of this node
     * 
     * @return
     */
    public FibonacciHeapNode<T> getRight() {
        return right;
    }

    /**
     * Decrease this node's degree (should be called after removing a child)
     */
    public void decDegree() {
        degree--;
    }

    /**
     * Retrieves the degree (number of childs) of this node
     * 
     * @return
     */
    public int getDegree() {
        return degree;
    }

    /**
     * Sets the given child as the entry point in the new list of this node's
     * childs
     * 
     * @param child
     */
    public void setChild(FibonacciHeapNode<T> child) {
        this.child = child;
    }

    /**
     * Retrieves the child of this node
     * 
     * @return The child of this node
     */
    public FibonacciHeapNode<T> getChild() {
        return child;
    }

    /**
     * Marks this node
     * 
     * @param mark
     */
    public void setMark(boolean mark) {
        this.mark = mark;
    }

    /**
     * Checks whether this node is marked
     * 
     * @return
     */
    public boolean isMark() {
        return mark;
    }

    /**
     * Remove this node from the list it appears in
     */
    public void remove() {
        left.setRight(right);
        right.setLeft(left);
    }

    /**
     * Adds x to the right of this node in the list
     * 
     * @param x
     */
    public void add(FibonacciHeapNode<T> x) {
        x.left = this;
        x.right = right;
        right = x;
        x.right.left = x;
    }

    /**
     * Make node y a child of this node.
     * <p>
     * Running time: O(1) actual
     * </p>
     * 
     * @param y
     *            node to become child
     */
    public void link(FibonacciHeapNode<T> y) {
        y.remove();

        // make y a child of x
        y.setParent(this);

        if (child == null) {
            child = y;
            y.setRight(y);
            y.setLeft(y);
        } else {
            child.add(y);
        }

        // increase degree[x]
        degree++;
    }

}