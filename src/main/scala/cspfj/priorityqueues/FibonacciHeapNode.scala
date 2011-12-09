package cspfj.priorityqueues
import scala.collection.mutable.DoubleLinkedList

/**
 * Implements a node of the Fibonacci heap. It holds the information
 * necessary for maintaining the structure of the heap. It also holds the
 * reference to the key value (which is used to determine the heap
 * structure).
 *
 * @author Nathan Fiedler, Julien Vion
 */
trait FibonacciHeapNode[T <: FibonacciHeapNode[T]] extends PTag with LazyKey with DLLNode[T] { self: T =>

  var child: Option[T] = None

  var parent: Option[T] = None

  var degree = 0

  var mark = false

  def clear() {
    mark = false
    degree = 0
    child = None
    parent = None
  }

  /**
   * Make this node a child of the given parent node. All linkages are
   * updated, the degree of the parent is incremented, and mark is set to
   * false.
   *
   * @param parent
   *            the new parent node.
   */
  def link(parent: T) {
    /**
     * Note: putting this code here in Node makes it 7x faster because
     * it doesn't have to use generated accessor methods, which add a
     * lot of time when called millions of times.
     */

    // Remove this from its circular list
    left.right = right;
    right.left = left;
    // make this a child of x
    this.parent = Some(parent);
    parent.child match {
      case None => {
        parent.child = Some(this);
        right = this;
        left = this;
      }
      case Some(child) => {
        left = child;
        right = child.right;
        child.right = this;
        right.left = this;
      }
    }

    // increase degree[x]
    parent.degree += 1;
    // set mark false
    mark = false;
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
  def cut(x: T, min: T) {
    // remove x from childlist and decrement degree
    x.left.right = x.right;
    x.right.left = x.left;
    degree -= 1;
    // reset child if necessary
    if (degree == 0) {
      child = None;
    } else if (child == x) {
      child = Some(x.right);
    }
    // add x to root list of heap
    x.right = min;
    x.left = min.left;
    min.left = x;
    x.left.right = x;
    // set parent[x] to nil
    x.parent = None;
    // set mark[x] to false
    x.mark = false;
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
  def cascadingCut(min: T) {
    // if there's a parent...
    if (parent.isDefined) {
      val z = parent.get
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

  def viewFHN = {

    val buf = new StringBuilder();
    buf.append("Node=[");
    if (left != null) {
      buf.append(left);
    } else {
      buf.append(" * ");
    }
    buf.append(" <- ").append(this).append(" -> ");
    if (right != null) {
      buf.append(right);
    } else {
      buf.append(" * ");
    }

    buf.append(", ^: ");

    if (parent != null) {
      buf.append(parent);
    } else {
      buf.append(" * ");
    }

    buf.append(", v: ");

    if (child != null) {
      buf.append(child);
    } else {
      buf.append(" * ");
    }

    buf.append(" (").append(degree).append(")]");

    buf.toString;

  }

}