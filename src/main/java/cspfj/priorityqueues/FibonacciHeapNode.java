package cspfj.priorityqueues;

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

    private int key;

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
        right = this;
        left = this;
        clear();
    }

    // ~ Methods
    // ----------------------------------------------------------------

    public int getKey() {
        return key;
    }

    public void setKey(int key) {
        this.key = key;
    }

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

        final StringBuilder buf = new StringBuilder();
        buf.append("Node=[");
        if (left != null) {
            buf.append(getLeft().data);
        } else {
            buf.append(" * ");
        }
        buf.append(" <- ").append(data).append(" -> ");
        if (right != null) {
            buf.append(getRight().data);
        } else {
            buf.append(" * ");
        }

        buf.append(", ^: ");

        if (parent != null) {
            buf.append(getParent().data);
        } else {
            buf.append(" * ");
        }

        buf.append(", v: ");

        if (child != null) {
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
        left.right = right;
        right.left = left;
    }

    /**
     * Adds x to the left of this node in the list
     * 
     * @param x
     */
    public void add(FibonacciHeapNode<T> x) {
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
    public void link(FibonacciHeapNode<T> parent) {
        // Note: putting this code here in Node makes it 7x faster
        // because it doesn't have to use generated accessor methods,
        // which add a lot of time when called millions of times.
        // remove this from its circular list
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
     * Performs a cascading cut operation. Cuts this from its parent and then
     * does the same for its parent, and so on up the tree.
     * 
     * <p>
     * <em>Running time: O(log n)</em>
     * </p>
     * 
     * @param min
     *            the minimum heap node, to which nodes will be added.
     */
    public void cascadingCut(FibonacciHeapNode<T> min) {
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
     * The reverse of the link operation: removes x from the child list of this
     * node.
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
    public void cut(FibonacciHeapNode<T> x, FibonacciHeapNode<T> min) {
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

}