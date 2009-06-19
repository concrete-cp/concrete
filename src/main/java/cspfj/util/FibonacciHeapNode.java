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
public class FibonacciHeapNode<T> {
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
        clear();
    }

    public void clear() {
        mark = false;
        degree = 0;
        setRight(this);
        setLeft(this);
        child = null;
        parent = null;
    }

    // ~ Methods
    // ----------------------------------------------------------------

    /**
     * Obtain the data for this node.
     */
    public final T getData() {
        return data;
    }

    public void setParent(FibonacciHeapNode<T> parent) {
        this.parent = parent;
    }

    public FibonacciHeapNode<T> getParent() {
        return parent;
    }

    /**
     * Return the string representation of this object.
     * 
     * @return string representing this object
     */
    public String toString() {

        StringBuilder buf = new StringBuilder();
        buf.append("Node=[parent = ");

        if (getParent() != null) {
            buf.append(getParent().data);
        } else {
            buf.append("---");
        }

        buf.append(", degree = ");
        buf.append(Integer.toString(getDegree()));
        buf.append(", right = ");

        if (getRight() != null) {
            buf.append(getRight().data);
        } else {
            buf.append("---");
        }

        buf.append(", left = ");

        if (getLeft() != null) {
            buf.append(getLeft().data);
        } else {
            buf.append("---");
        }

        buf.append(", child = ");

        if (getChild() != null) {
            buf.append(getChild().data);
        } else {
            buf.append("---");
        }

        buf.append(']');

        return buf.toString();

    }

    public void setLeft(FibonacciHeapNode<T> left) {
        this.left = left;
    }

    public FibonacciHeapNode<T> getLeft() {
        return left;
    }

    public void setRight(FibonacciHeapNode<T> right) {
        this.right = right;
    }

    public FibonacciHeapNode<T> getRight() {
        return right;
    }

    public void setDegree(int degree) {
        this.degree = degree;
    }

    public int getDegree() {
        return degree;
    }

    public void setChild(FibonacciHeapNode<T> child) {
        this.child = child;
    }

    public FibonacciHeapNode<T> getChild() {
        return child;
    }

    public void setMark(boolean mark) {
        this.mark = mark;
    }

    public boolean isMark() {
        return mark;
    }

}