package cspfj.priorityqueues
import scala.collection.mutable.DoubleLinkedList
import scala.annotation.tailrec

/**
 * Implements a node of the Fibonacci heap. It holds the information
 * necessary for maintaining the structure of the heap. It also holds the
 * reference to the key value (which is used to determine the heap
 * structure).
 *
 * @author Nathan Fiedler, Julien Vion
 */
trait FibonacciHeapNode[T >: Null <: FibonacciHeapNode[T]] extends PTag with LazyKey[T] with BinomialHeapNode[T] { self: T =>

  var mark = false

  override def clearNode() {
    super.clearNode()
    mark = false
  }

  /**
   * Make this node a child of the given parent node. All linkages are
   * updated, the degree of the parent is incremented, and mark is set to
   * false.
   *
   * @param parent
   *            the new parent node.
   */
  def link(tree: T) {
    remove()
    clearDL()
    tree.addSubTree(this)
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
    x.remove()
    //    x.left.right = x.right;
    //    x.right.left = x.left;
    rank -= 1;
    // reset child if necessary
    if (rank == 0) {
      child = null;
    } else if (child == x) {
      child = x.right
    }
    // add x to root list of heap
    min.add(x)
    // set parent[x] to nil
    x.parent = null;
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
  @tailrec
  private def cascadingCut(min: T) {
    val z = parent
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

  def decrease(child: T, minNode: T) {
    cut(child, minNode)
    cascadingCut(minNode)
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

    buf.append(" (").append(rank).append(")]");

    buf.toString;

  }

}