package concrete.priorityqueues;

import java.util.Arrays
import concrete.Statistic
import scala.annotation.tailrec

final class ScalaBinomialHeap[T <: Identified](initSize: Int) extends PriorityQueue[T] {

  @Statistic
  private var nbOffer = 0;
  @Statistic
  private var nbUpdate = 0;
  @Statistic
  private var nbPoll = 0;
  @Statistic
  private var nbClear = 0;
  @Statistic
  private var offerSize = 0L;
  @Statistic
  private var updateSize = 0L;
  @Statistic
  private var pollSize = 0L;

  /* 2 + log_2(Integer.MAX_VALUE) */
  private val MAX_ARRAY_SIZE = 33;

  // private final Key<T> key;

  private val trees = new Array[BinomialHeapNode](initSize)

  private var size = 0;

  private var map = new Array[BinomialHeapNode](MAX_ARRAY_SIZE)

  private var iter = 0;

  private var last = -1;

  // @Statistic
  // public static int insert = 0;
  // @Statistic
  // public static int update = 0;
  // @Statistic
  // public static int remove = 0;

  def this() = this(100);

  /**
   * Increases the capacity of this instance to ensure that it can hold at
   * least the number of elements specified by the minimum capacity argument.
   *
   * @param minCapacity
   *            the desired minimum capacity
   */
  private def ensureCapacity(minCapacity: Int) {
    val oldCapacity = map.length;
    if (minCapacity > oldCapacity) {
      val newCapacity = math.max(minCapacity, (oldCapacity * 3) / 2 + 1);
      // minCapacity is usually close to size, so this is a win:

      map = Arrays.copyOf(map, newCapacity)
    }

  }

  def offer(arg0: T, newKey: Int) = {
    val id = arg0.getId

    var node =
      try map(id)
      catch {
        case e: ArrayIndexOutOfBoundsException =>
          ensureCapacity(id + 1)
          map(id)
      }

    if (node == null) {
      node = new BinomialHeapNode(arg0);
      map(id) = node;
    }

    val oldKey = node.key;

    node.key = newKey;

    if (node.isPresent(iter)) {
      nbUpdate += 1;
      updateSize += size;
      if (newKey < oldKey) {
        node.decreaseKey(false);
      } else if (newKey > oldKey) {
        increaseKey(node);
      }
      false;
    } else {
      nbOffer += 1;
      offerSize += size;
      node.clear();

      carryMerge(node, 0);
      node.setPresent(iter);
      size += 1;
      true;
    }
  }

  def poll(): T = {
    val min = minTree();
    deleteRoot(min);
    min.unsetPresent();
    nbPoll += 1;
    pollSize += size;
    size -= 1;
    min.data;
  }

  def isEmpty = size == 0

  @tailrec
  private def mergeAll(node: BinomialHeapNode) {
    if (node != null) {
      val next = node.right
      node.right = null
      node.parent = null
      carryMerge(node, node.rank)
      mergeAll(next)
    }
  }

  private def deleteRoot(root: BinomialHeapNode) {
    assert(root.parent == null)
    trees(root.rank) = null
    mergeAll(root.child)
  }

  private def minTree() = {
    var min = trees(last)
    while (min == null) {
      last -= 1
      min = trees(last)
    }

    var i = last - 1
    while (i >= 0) {
      val tree = trees(i);
      if (tree != null && tree < min) {
        min = tree;
      }
      i -= 1
    }
    min;
  }

  def clear() {
    while (last >= 0) {
      trees(last) = null
      last -= 1
    }

    iter += 1;
    size = 0;
    nbClear += 1;
  }

  @tailrec
  private def carryMerge(tree: BinomialHeapNode, i: Int) {
    trees(i) match {
      case null =>
        trees(i) = tree
        last = math.max(last, i)
      case storedTree =>
        trees(i) = null

        /**
         * We merge either the stored tree under the given tree or conversely to
         * maintain heap property.
         */
        if (storedTree < tree) {
          storedTree.addSubTree(tree);
          carryMerge(storedTree, i + 1);
        } else {
          tree.addSubTree(storedTree);
          carryMerge(tree, i + 1);
        }
    }

  }

  private def increaseKey(node: BinomialHeapNode) {
    val root = node.decreaseKey(true);
    deleteRoot(root);
    root.clear();
    carryMerge(root, 0);
  }

  override def toString = trees.mkString("\n")

  private final class BinomialHeapNode(var data: T) extends Ordered[BinomialHeapNode] {

    var key: Int = 0

    var child: BinomialHeapNode = null

    var right: BinomialHeapNode = null

    var parent: BinomialHeapNode = null

    var rank = 0

    private var inQueue = -1

    @tailrec
    def decreaseKey(delete: Boolean): BinomialHeapNode = {
      if (parent != null && (delete || this < parent)) {
        swap(parent)
        parent.decreaseKey(delete)
      } else this
    }

    def compare(c: BinomialHeapNode) = key - c.key

    override def <(c: BinomialHeapNode) = key < c.key

    private def swap(node: BinomialHeapNode) {
      val data = this.data
      map(data.getId) = node
      map(node.data.getId) = this
      val key = this.key
      this.data = node.data
      this.key = node.key
      node.data = data
      node.key = key

    }

    def clear() {
      child = null;
      right = null;
      parent = null;
      rank = 0;
    }

    def addSubTree(subTree: BinomialHeapNode) {
      if (child != null) {
        subTree.right = child;
      }
      rank += 1;
      child = subTree;
      subTree.parent = this;
    }

    override def toString() = {
      val stb = new StringBuilder();
      tree(stb, 0);
      stb.toString();
    }

    private def tree(stb: StringBuilder, depth: Int) {
      (0 until depth).foreach(stb.append("--"))

      stb.append(data).append(" (").append(key).append(", ").append(rank)
        .append(")\n");
      if (child != null) {
        child.tree(stb, depth + 1);
      }
      if (right != null) {
        right.tree(stb, depth);
      }
    }

    def setPresent(iter: Int) {
      inQueue = iter;
    }

    def unsetPresent() {
      inQueue = -1;
    }

    def isPresent(iter: Int) = inQueue == iter

  }

}
