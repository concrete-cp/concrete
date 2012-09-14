package cspfj.constraint.extension

import scala.annotation.tailrec

object MDD {
  val leaf = new MDDNode(Array(), 1)

  val empty = new MDDNode(Array(), 0)

  def apply(data: Array[Int]*) = {
    data.foldLeft(new MDD(Map(), empty))(_ + _)
  }
}

class MDD(var mdds: Map[Seq[MDDNode], MDDNode], val root: MDDNode) {

  var timestamp = 0

  def obtain(trie: Array[MDDNode], size: Int) = {
    mdds.get(trie) match {
      case Some(node) => {
        require(node.size == size)
        node
      }
      case None => {
        val v = new MDDNode(trie, size)
        mdds += trie.toSeq -> v
        v
      }
    }
  }

  def size = root.size

  def +(t: Array[Int]) = new MDD(mdds, root + (this, t))

  def filterTrie(f: (Int, Int) => Boolean, modified: Seq[Int], depth: Int = 0) = {
    timestamp += 1
    new MDD(mdds, root.filterTrie(this, f, modified, depth))
  }

  def contains(t: Array[Int]) = root.contains(t)

  def iterator = root.iterator

  def setFound(f: (Int, Int) => Int, maxDepth: Int): Int = {
    timestamp += 1
    root.setFound(timestamp, f, maxDepth, 0)
  }

  def nodes = {
    timestamp += 1
    root.nodes(timestamp)
  }

  override def toString = {
    timestamp += 1
    root.toString(timestamp)
  }

}

final class MDDNode(val trie: Array[MDDNode], val size: Int) {

  var timestamp = 0

  def depth: Int = {
    if (this eq MDD.leaf) 0
    else {
      1 + values.map(_.depth).max
    }
  }

  private def values = trie.filter(_ ne null)

  def isEmpty = size == 0

  def +(mdd: MDD, t: Array[Int]): MDDNode = if (contains(t)) this else this + (mdd, t, 0)

  //  /**
  //   * Increases the capacity of this instance, if necessary, to ensure that it
  //   * can hold at least the number of elements specified by the minimum
  //   * capacity argument.
  //   *
  //   * @param minCapacity
  //   *            the desired minimum capacity
  //   */
  //  private def ensureCapacity(minCapacity: Int) {
  //    val oldCapacity = trie.length;
  //    if (minCapacity > oldCapacity) {
  //      val newCapacity = math.max(minCapacity, (oldCapacity * 3) / 2 + 1);
  //      // minCapacity is usually close to size, so this is a win:
  //      trie = Arrays.copyOf(trie, newCapacity)
  //    }
  //  }

  private def +(mdd: MDD, tuple: Array[Int], i: Int): MDDNode = {
    require(i < tuple.length)
    val v = tuple(i)
    val newArray = trie.padTo(v + 1, null)
    //ensureCapacity(v + 1)

    if (newArray(v) eq null) {
      newArray(v) =
        if (i == tuple.length - 1) MDD.leaf
        else MDD.empty + (mdd, tuple, i + 1)
    } else {
      newArray(v) += (mdd, tuple, i + 1)
    }
    mdd.obtain(newArray, size + 1)
  }

  //  def -(t: Array[Int]): MDDNode = if (contains(t)) (this - (t, 0)) else this
  //
  //  private def -(tuple: Array[Int], i: Int): MDDNode = {
  //    if (i < tuple.length) {
  //      val v = tuple(i)
  //
  //      val newArray = trie.clone
  //      val t = trie(v)
  //
  //      if (t eq mdd.leaf) {
  //        newArray(v) = null
  //      } else {
  //        val newTrie = t - (tuple, i + 1)
  //        if (newTrie.isEmpty) newArray(v) = null else newArray(v) = newTrie
  //      }
  //      new MDDNode(mdd, newArray, size - 1)
  //    }
  //    this
  //  }

  def contains(tuple: Array[Int]): Boolean = contains(tuple, 0)

  @tailrec
  private def contains(tuple: Array[Int], i: Int): Boolean = {
    if (i >= tuple.length) true
    else if (tuple(i) >= trie.length) false
    else trie(tuple(i)) match {
      case null => false
      case t => t.contains(tuple, i + 1)
    }
  }

  override def equals(o: Any): Boolean = o match {
    case t: MDDNode => trie sameElements t.trie
    case _ => false
  }

  override lazy val hashCode = trie.toSeq.hashCode

  def toString(ts: Int) = nodes(ts) + " nodes representing " + size + " tuples" // + toString(0)

  def foreachTrie(f: (Int, Int) => Unit, depth: Int = 0) {
    var i = trie.length - 1;
    while (i >= 0) {
      if (trie(i) ne null) {
        f(depth, i)
        trie(i).foreachTrie(f, depth + 1)
      }
      i -= 1
    }
  }

  /**
   * @param f(depth, i) : function called while traversing the trie, given depth and index. Returns the depth at which traversing must be stopped
   * @param maxDepth : initial maximum depth at which traversing must be stopped
   * @param depth : current depth
   * @return current maximum depth
   */
  def setFound(ts: Int, f: (Int, Int) => Int, maxDepth: Int, depth: Int = 0): Int = {
    if (timestamp == ts) maxDepth
    else {
      timestamp = ts
      var i = trie.length - 1
      var last = maxDepth
      while (i >= 0 && depth <= last) {
        if (trie(i) ne null) {
          last = f(depth, i)
          last = trie(i).setFound(ts, f, last, depth + 1)
        }
        i -= 1
      }
      last
    }
  }

  def filterTrie(mdd: MDD, f: (Int, Int) => Boolean, modified: Seq[Int], depth: Int = 0): MDDNode = {
    if (modified.isEmpty || timestamp == mdd.timestamp) this
    else {
      timestamp = mdd.timestamp
      var newTrie: Array[MDDNode] = null
      var i = trie.length - 1
      var newSize = 0

      if (modified.head == depth) {
        // Some change at this level
        while (i >= 0) {
          val currentTrie = trie(i)
          if ((currentTrie ne null) && f(depth, i)) {

            val newSubTrie = currentTrie.filterTrie(mdd, f, modified.tail, depth + 1)
            if (newSubTrie ne null) {
              if (newTrie eq null) {
                newTrie = new Array[MDDNode](i + 1)
              }
              newTrie(i) = newSubTrie
              newSize += newSubTrie.size
            }
          }
          i -= 1
        }
      } else {
        // No change at this level (=> no need to call f())
        while (i >= 0) {
          val currentTrie = trie(i)
          if (currentTrie ne null) {
            val newSubTrie = currentTrie.filterTrie(mdd, f, modified, depth + 1)
            if (newSubTrie ne null) {
              if (newTrie eq null) {
                newTrie = new Array[MDDNode](i + 1)
              }
              newTrie(i) = newSubTrie
              newSize += newSubTrie.size
            }
          }
          i -= 1
        }
      }

      if (newSize == 0) null
      else if (size == newSize) this
      else mdd.obtain(newTrie, newSize)

    }
  }

  //  private def asStream: Stream[List[Int]] =
  //    trie.toStream flatMap {
  //      case (i, t) => if (t.isEmpty) Stream(List(i)) else t.asStream map (i :: _)
  //    }

  private def listiterator: Iterator[List[Int]] = trie.iterator.zipWithIndex flatMap {
    case (t, i) if (t ne null) => if (t eq MDD.leaf) Iterator(List(i)) else t.listiterator map (i :: _)
    case _ => Nil
  }

  def iterator = listiterator map (_.toArray)
  //  
  //  private def asIterable: Iterable[List[Int]] = trie flatMap {
  //    case (i, t) => if (t.isEmpty) List(List(i)) else t.asIterable map (i :: _)
  //  } 
  //  
  //  override def toList = asList map (_.toArray)

  def nodes(ts: Int): Int = {
    if (ts == timestamp) 0
    else {
      timestamp = ts
      1 + values.map(_.nodes(ts)).sum
    }
  }

  def tupleString = iterator map { _.mkString(" ") } mkString "|"

}

