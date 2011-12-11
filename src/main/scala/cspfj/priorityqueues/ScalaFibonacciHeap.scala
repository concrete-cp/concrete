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
final class ScalaFibonacciHeap[T >: Null <: FibonacciHeapNode[T]](
  val key: Key[T]) extends AbstractQueue[T] {
  /**
   * The magic 45 comes from log base phi of Integer.MAX_VALUE, which is the
   * most elements we will ever hold, and log base phi represents the largest
   * degree of any root list node.
   */
  private val MAX_ARRAY_SIZE = 45;

  private val DEFAULT_SIZE = 10;

  def init: Stream[FibonacciHeapNode[T]] = null #:: init

  private val array = init.take(MAX_ARRAY_SIZE).toArray.asInstanceOf[Array[T]] //.padTo(MAX_ARRAY_SIZE, null) //maxNbTrees(DEFAULT_SIZE))

  /**
   * Points to the minimum node in the heap.
   */
  private var minNode: T = null

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
  override def isEmpty = minNode == null;

  /**
   * Removes all elements from this heap.
   */
  override def clear() {
    minNode = null;
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
    val y = x.parent
    if (y != null && (delete || x < y)) {
      y.decrease(x, minNode);
    }
    if (delete || x < minNode) {
      minNode = x
    }

    assert(control(minNode, minNode))
  }

  /**
   * Key increase is implemented as deletion and reinsertion of the node.
   *
   * @param x
   */
  private def increaseKey(x: T) {
    decreaseKey(x, true);
    removeMin();
    x.clearNode();
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
    if (minNode == null) {
      minNode = node
      /**
       * Should not be required as node has
       * been cleared before
       */
      //node.left = node
      //node.right = node
    } else {
      minNode.add(node)
      if (node < minNode) {
        minNode = node
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
      node.clearNode();
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
    val z = minNode
    if (z == null) {
      null
    } else {
      val zChild = z.child
      if (zChild != null) {
        zChild.parent = null;
        // for each child of z do...
        var x = zChild.right
        while (x != zChild) {
          // set parent[x] to null
          x.parent = null;
          x = x.right
        }
        // merge the children into root list

        minNode.merge(zChild)
      }
      // remove z from root list of heap
      z.remove();
      if (z == z.right) {
        minNode = null;
      } else {
        minNode = z.right
        consolidate();
      }
      // decrement size of heap
      nNodes -= 1;

      assert(control(minNode, minNode));
      z

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
  def emptyArray(array: Array[T], i: Int) {
    if (i >= 0) {
      array(i) = null
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
    var start = minNode
    var w = minNode
    do {
      var x = w;
      // Because x might be moved, save its sibling now.
      var nextW = w.right;
      var d = x.rank;
      while (array(d) != null) {
        // Make one of the nodes a child of the other.
        var y = array(d)
        if (x.key > y.key) {
          val temp = y;
          y = x;
          x = temp;
        }
        if (y == start) {
          /**
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
        array(d) = null;
        d += 1;
      }
      // Save this node for later when we might encounter another
      // of the same degree.
      array(d) = x;
      // Move forward through list.
      w = nextW;
    } while (w != start);

    // The node considered to be min may have been changed above.
    minNode = minTree(array.length - 1, null) //Some(start);
    // Find the minimum key again.
    //    for (a <- array) {
    //      if (a != null && a.key < minNode.key) {
    //        minNode = a;
    //      }
    //    }

    assert(control(minNode, minNode));
  }

  @tailrec
  private def minTree(i: Int, min: T): T = {
    if (i < 0) {
      min
    } else {
      val t = array(i)

      if (t == null || min != null && min < t) minTree(i - 1, min)
      else minTree(i - 1, t)

    }

  }

  def iterator =
    throw new UnsupportedOperationException();

  def peek = minNode

  def poll() = {
    removals += 1
    val min = removeMin();
    min.unsetPresent()
    min
  }

  private def control(current: FibonacciHeapNode[T],
    start: FibonacciHeapNode[T]): Boolean =
    true || control(current, start, Set(), Set());

  private def control(current: FibonacciHeapNode[T],
    start: FibonacciHeapNode[T],
    loopControl: Set[FibonacciHeapNode[T]],
    ancestorControl: Set[FibonacciHeapNode[T]]): Boolean = {
    if (current == null) {
      true;
    } else {
      assert(!loopControl(current))
      assert(!ancestorControl(current))

      var ac = ancestorControl

      if (current.child != null) {
        ac += current

        if (!control(current.child, current.child, Set(), ac)) {
          return false;
        }
      }

      current.right == start || control(current.right, start, loopControl + current, ac);
    }
  }

  /**
   * Creates a String representation of this Fibonacci heap.
   *
   * @return String of this.
   */
  override def toString = {
    if (minNode == null) {
      "empty";
    } else {
      val stb = new StringBuilder();
      tree(stb, minNode, minNode, 0);
      stb.toString
    }
  }

  private def tree(stb: StringBuilder,
    current: T,
    start: T, depth: Int) {

    (0 until depth).foreach(stb.append("--"))
    stb.append(current).append("\n");
    if (current.child != null) {
      tree(stb, current.child, current.child, depth + 1);
    }
    if (current.right != start) {
      tree(stb, current.right, start, depth);
    }
  }

}
