/**
 * --------------------------
 * FibonnaciHeap.java
 * --------------------------
 * (C) Copyright 1999-20089, by Julien Vion, Nathan Fiedler and Contributors.
 *
 * Original Author:  Nathan Fiedler
 */
package cspfj.priorityqueues;

import scala.annotation.tailrec
import java.util.AbstractQueue
import java.util.Arrays

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
 * @author Nathan Fiedler, Julien Vion
 */
final class ScalaFibonacciHeap[T <: FibonacciHeapNode[T]](
  val key: Key[T]) extends AbstractQueue[T] {
  /**
   * The magic 45 comes from log base phi of Integer.MAX_VALUE, which is the
   * most elements we will ever hold, and log base phi represents the largest
   * degree of any root list node.
   */
  private val MAX_ARRAY_SIZE = 45;

  private val DEFAULT_SIZE = 10;

  private val array = Array[Option[T]]().padTo(MAX_ARRAY_SIZE, None)

  /**
   * Points to the minimum node in the heap.
   */
  private var minNode: Option[T] = None

  /**
   * Number of nodes in the heap.
   */
  private var nNodes = 0

  var insert = 0;
  var update = 0;
  var removals = 0;

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
  override def isEmpty = minNode == None;

  /**
   * Removes all elements from this heap.
   */
  override def clear() {
    minNode = None;
    nNodes = 0;
    PTag.clear()
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
  private def decreaseKey(x: T, delete: Boolean) {
    x.parent match {
      case Some(y) if delete || x.key < y.key => {
        y.cut(x, minNode.get);
        y.cascadingCut(minNode.get);
      }
      case None =>
    }
    if (delete || x.key < minNode.get.key) {
      minNode = Some(x);
    }

    //assert(control(minNode, minNode))
  }

  /**
   * Key increase is implemented as deletion and reinsertion of the node.
   *
   * @param x
   */
  private def increaseKey(x: T) {
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
  private def insert(node: T) {
    // concatenate node into min list
    minNode match {
      case Some(n) => {
        n.add(node)
        if (node.key < n.key) {
          minNode = Some(node)
        }
      }
      case None => {
        minNode = Some(node)
      }

    }

    nNodes += 1;
  }

  def offer(node: T) = {

    val oldKey = node.key;
    val newKey = key.getKey(node);
    node.key = newKey;

    if (node.isPresent) {
      update += 1;
      if (newKey < oldKey) {
        decreaseKey(node, false);
        //assert(smallest(minNode));
      } else if (newKey > oldKey) {
        increaseKey(node);
        //assert(smallest(minNode));
      }

      false;
    } else {
      insert += 1
      node.clear();
      insert(node);
      node.setPresent()
      true;
    }
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
  def removeMin() = {
    minNode match {
      case None => None
      case Some(z) => {

        if (z.child.isDefined) {
          val zChild = z.child.get
          zChild.parent = None;
          // for each child of z do...
          var x = zChild.right
          while (x != zChild) {
            // set parent[x] to null
            x.parent = null;
            x = x.right
          }
          // merge the children into root list

          val minLeft = z.left;
          val zChildLeft = zChild.left;
          z.left = zChildLeft;
          zChildLeft.right = z;
          zChild.left = minLeft;
          minLeft.right = zChild;
        }
        // remove z from root list of heap
        z.remove();
        if (z == z.right) {
          minNode = None;
        } else {
          minNode = Some(z.right)
          consolidate();
        }
        // decrement size of heap
        nNodes -= 1;

        //assert(control(minNode, minNode));
        Some(z);

      }
    }
  }

  //    private def smallest(min: FibonacciHeapNode[T]) =
  //      {
  //
  //        for (FibonacciHeapNode<T> n : map) {
  //            if (n != null && n.inQueue == iter && n.key < min.key) {
  //                return false;
  //            }
  //        }
  //
  //        return true;
  //    }

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
  def size = nNodes;

  @tailrec
  def emptyArray(array: Array[Option[T]], i: Int) {
    if (i >= 0) {
      array(i) = None
      emptyArray(array, i - 1)
    }
  }

  /**
   * Consolidates the trees in the heap by joining trees of equal degree until
   * there are no more trees of equal degree in the root list.
   *
   * <p>
   * <em>Running time: O(log n) amortized</em>
   * </p>
   */
  private def consolidate() {
    emptyArray(array, array.length - 1)

    // For each root list node look for others of the same degree.
    var start = minNode.get
    var w = minNode.get
    do {
      var x = w;
      // Because x might be moved, save its sibling now.
      var nextW = w.right;
      var d = x.degree;
      while (array(d).isDefined) {
        // Make one of the nodes a child of the other.
        var y = array(d).get
        if (x.key > y.key) {
          val temp = y;
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
        array(d) = None;
        d += 1;
      }
      // Save this node for later when we might encounter another
      // of the same degree.
      array(d) = Some(x);
      // Move forward through list.
      w = nextW;
    } while (w != start);

    // The node considered to be min may have been changed above.
    minNode = Some(array.iterator.flatten.minBy(_.key)) //Some(start);
    // Find the minimum key again.
    //    for (a <- array) {
    //      if (a != null && a.key < minNode.key) {
    //        minNode = a;
    //      }
    //    }

    //assert(control(minNode, minNode));
  }

  def iterator =
    throw new UnsupportedOperationException();

  def peek = minNode.asInstanceOf[T]

  def poll() = {
    removals += 1
    val min = removeMin().get;
    min.unsetPresent()
    min
  }

  //  private def control(current: FibonacciHeapNode[T],
  //    start: FibonacciHeapNode[T]): Boolean =
  //    control(current, start, Set(), Set());
  //
  //  private def control(current: FibonacciHeapNode[T],
  //    start: FibonacciHeapNode[T],
  //    loopControl: Set[FibonacciHeapNode[T]],
  //    ancestorControl: Set[FibonacciHeapNode[T]]): Boolean = {
  //    if (current == null) {
  //      true;
  //    } else {
  //      //assert(!loopControl(current))
  //      //assert(!ancestorControl(current))
  //
  //      var ac = ancestorControl
  //
  //      if (current.child != null) {
  //        ac += current
  //
  //        if (!control(current.child, current.child, Set(), ac)) {
  //          return false;
  //        }
  //      }
  //
  //      current.right == start || control(current.right, start, loopControl + current, ac);
  //    }
  //  }

  /**
   * Creates a String representation of this Fibonacci heap.
   *
   * @return String of this.
   */
  def fiboToString = {
    if (minNode == None) {
      "empty";
    } else {
      val stb = new StringBuilder();
      tree(stb, minNode.get, minNode.get, 0);
      stb.toString
    }
  }

  private def tree(stb: StringBuilder,
    current: T,
    start: T, depth: Int) {

    (0 until depth).foreach(stb.append("--"))
    stb.append(current).append("\n");
    if (current.child.isDefined) {
      tree(stb, current.child.get, current.child.get, depth + 1);
    }
    if (current.right != start) {
      tree(stb, current.right, start, depth);
    }
  }

}
