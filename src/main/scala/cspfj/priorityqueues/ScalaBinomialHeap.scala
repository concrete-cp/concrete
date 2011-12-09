package cspfj.priorityqueues

import java.util.AbstractQueue
import java.util.Arrays
import java.util.Iterator
import cspfj.Statistic
import scala.annotation.tailrec

final class ScalaBinomialHeap[T <: BinomialHeapNode[T]](key: Key[T]) extends AbstractQueue[T] {

  //  val DEFAULT_SIZE = 10;
  //
  //  val LOG2 = math.log(2);

  private var trees = Array[Option[T]]().padTo(10, None) //maxNbTrees(DEFAULT_SIZE))

  private var _size = 0;

  @Statistic
  var insert = 0;
  @Statistic
  var update = 0;
  @Statistic
  var removals = 0;

  //  private def maxNbTrees(size: Int) = (2 + math.floor(math.log(size) / LOG2)).toInt

  def iterator() = {
    throw new UnsupportedOperationException();
  }

  override def size = _size

  def offer(arg0: T) = {

    val oldKey = arg0.key;
    val newKey = key.getKey(arg0);
    arg0.key = newKey;

    if (arg0.isPresent) {
      update += 1;
      //            if (newKey < oldKey) {
      //                decreaseKey(arg0, false);
      //            } else if (newKey > oldKey) {
      //                increaseKey(arg0);
      //            }
      false;
    } else {
      insert += 1;
      arg0.clearNode();

      carryMerge(arg0, 0);
      arg0.setPresent
      _size += 1;
      true;
    }
  }

  def peek = minTree.asInstanceOf[T]

  def poll() = {
    removals += 1;
    val min = minTree;
    deleteRoot(min);
    min.unsetPresent()
    _size -= 1;
    min.asInstanceOf[T];
  }

  @tailrec
  def distribute(subTree: T, last: T) {
    val next = subTree.right
    subTree.remove()
    subTree.parent = None
    carryMerge(subTree, subTree.rank)
    if (next != last) {
      distribute(next, last)
    }
  }

  private def deleteRoot(root: BinomialHeapNode[T]) {
    assert(root.parent.isEmpty)
    trees(root.rank) = None;

    if (root.child.isDefined)
      distribute(root.child.get, root.child.get)

  }

  @tailrec
  private def minTree(i: Int, min: Option[T]): Option[T] = {
    if (i < 0) {
      min
    } else trees(i) match {
      case None => minTree(i - 1, min)
      case Some(t) => if (min.isEmpty || t.key < min.get.key) {
        minTree(i - 1, Some(t))
      } else {
        minTree(i - 1, min)
      }
    }

  }

  private def minTree: T = minTree(trees.length - 1, None).get
  //  //minTree(trees.length - 1, null)
  //    trees.reduceLeft((t, min) => if (t == null) {
  //      min
  //    } else if (min == null || t.key < min.key) {
  //      t
  //    } else {
  //      min
  //    })

  override def clear() {
    trees.indices.foreach(i => trees(i) = null)
    //Arrays.fill(trees, null);
    PTag.clear()
    _size = 0;
  }

  private def carryMerge(tree: T, i: Int) {
    //ensureCapacity(i + 1)
    val storedTree = try trees(i)
    catch {
      case e: IndexOutOfBoundsException => {
        trees = trees.padTo(i + 1, None)
        None
      }
    }

    storedTree match {
      case None => trees(i) = Some(tree)
      case Some(sTree) => {

        trees(i) = None;

        /**
         * We merge either the stored tree under the given tree or the
         * reverse.
         */
        if (sTree.key <= tree.key) {
          sTree.addSubTree(tree);
          carryMerge(sTree, i + 1);
        } else {
          tree.addSubTree(sTree);
          carryMerge(tree, i + 1);
        }
      }
    }

  }

  //    private def decreaseKey(node: BinomialHeapNode[T], delete: Boolean): BinomialHeapNode[T] = {
  //      if (node.parent == null || (!delete && node.parent.key <= node.key)) {
  //        node
  //      } else {
  //        swap(node, node.parent)
  //        decreaseKey(node.parent, delete)
  //      }
  //    }
  //
  //    private def increaseKey(node: BinomialHeapNode[T]) {
  //        val root = decreaseKey(node, true);
  //        deleteRoot(root);
  //        root.clear();
  //        carryMerge(root, 0);
  //    }

  override def toString = trees.map {
    case None => "*"
    case Some(t) => t.tree(0, t)
  }.mkString("\n")

  //    private def swap(node1: BinomialHeapNode[T], node2: BinomialHeapNode[T]) {
  //      
  //        val node1Data = node1.data;
  //        final double node1Key = node1.key;
  //        map[node1Data.getId()] = node2;
  //        map[node2.data.getId()] = node1;
  //        node1.data = node2.data;
  //        node1.key = node2.key;
  //        node2.data = node1Data;
  //        node2.key = node1Key;
  //    }

}
