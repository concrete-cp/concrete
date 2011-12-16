package cspfj.priorityqueues

import java.util.AbstractQueue
import java.util.Iterator

import scala.annotation.tailrec

import cspfj.Statistic

final class ScalaIOBinomialHeap[T >: Null <: IOBinomialHeapNode[T]](
  val key: Key[T])
  extends AbstractQueue[T] {

  //  val DEFAULT_SIZE = 10;
  //
  //  val LOG2 = math.log(2);

  val MAX_ARRAY_SIZE = 33

  def init: Stream[IOBinomialHeapNode[T]] = null #:: init

  private val trees: Array[T] = init.take(MAX_ARRAY_SIZE).toArray.asInstanceOf[Array[T]] //.padTo(MAX_ARRAY_SIZE, null) //maxNbTrees(DEFAULT_SIZE))

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
      arg0.key = key.getKey(arg0)
      arg0.clearNode();
      carryMerge(arg0);
      arg0.setPresent()
      _size += 1;
      //assert(_size == computeSize)
      true;
    }
  }

  def peek = minTree

  def poll() = {
    removals += 1;
    val min = minTree;
    deleteRoot(min);
    min.unsetPresent()
    _size -= 1;
    //assert(_size == computeSize, _size + " should be " + computeSize)
    min
  }

  def computeSize: Int = {
    trees.iterator.filter(_ != null).map(t => t.treeSize).sum
  }

  @tailrec
  def distribute(subTree: T) {
    if (subTree != null) {
      val next = subTree.next
      subTree.next = null
      carryMerge(subTree)
      distribute(next)
    }
  }

  private def deleteRoot(root: IOBinomialHeapNode[T]) {
    trees(root.rank) = null;
    distribute(root.child)
  }
  //
  @tailrec
  private def minTree(i: Int, min: T): T = {
    if (i < 0) {
      min
    } else {
      val t = trees(i)

      if (t == null || min != null && min < t) minTree(i - 1, min)
      else minTree(i - 1, t)

    }

  }

  private def minTree: T = minTree(trees.length - 1, null) //trees.iterator.flatten.min //minTree(trees.length - 1, None).get
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

  private def carryMerge(tree: T) {

    val i = tree.rank
    val sTree = trees(i)

    if (sTree == null) trees(i) = tree
    else {

      trees(i) = null;

      /**
       * We merge either the stored tree under the given tree or the
       * reverse.
       */
      if (sTree < tree) {
        sTree.addSubTree(tree);
        carryMerge(sTree);
      } else {
        tree.addSubTree(sTree);
        carryMerge(tree);
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
    case null => "*"
    case t => t.tree(0)
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
