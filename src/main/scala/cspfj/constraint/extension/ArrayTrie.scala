package cspfj.constraint.extension

import scala.annotation.tailrec

object ArrayTrie {
  val empty = new ArrayTrie(Array(), 0)

  val leaf = new ArrayTrie(Array(), 1)

  def apply(tuples: Array[Int]*) = tuples.foldLeft(empty)(_ + _)
}

final class ArrayTrie(val trie: Array[ArrayTrie], override val size: Int) extends Set[Array[Int]] {

  def depth: Int = {
    if (this eq ArrayTrie.leaf) 0
    else {
      1 + values.map(_.depth).max
    }
  }

  private def values = trie.filter(_ ne null)

  override def isEmpty = size == 0

  def +(t: Array[Int]): ArrayTrie = if (contains(t)) this else this + (t, 0)

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

  private def +(tuple: Array[Int], i: Int): ArrayTrie = {
    if (i >= tuple.length) ArrayTrie.leaf
    else {
      val v = tuple(i)
      val newArray = trie.padTo(v + 1, null)
      //ensureCapacity(v + 1)

      if (newArray(v) eq null) {
        newArray(v) =
          if (i == tuple.length - 1) ArrayTrie.leaf
          else ArrayTrie.empty + (tuple, i + 1)
      } else {
        newArray(v) += (tuple, i + 1)
      }
      new ArrayTrie(newArray, size + 1)
    }
  }

  def -(t: Array[Int]): ArrayTrie = if (contains(t)) (this - (t, 0)) else this

  private def -(tuple: Array[Int], i: Int): ArrayTrie = {
    if (i < tuple.length) {
      val v = tuple(i)

      val newArray = trie.clone
      val t = trie(v)

      if (t eq ArrayTrie.leaf) {
        newArray(v) = null
      } else {
        val newTrie = t - (tuple, i + 1)
        if (newTrie.isEmpty) newArray(v) = null else newArray(v) = newTrie
      }
      new ArrayTrie(newArray, size - 1)
    }
    this
  }

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
    case t: ArrayTrie => trie sameElements t.trie
    case _ => false
  }

  override lazy val hashCode = trie.toSeq.hashCode

  override def toString = nodes + " nodes representing " + size + " tuples" // + toString(0)

  private def toString(depth: Int): String =
    trie.zipWithIndex.filter(_._1 ne null).map {
      case (v: ArrayTrie, k: Int) =>
        List.fill(depth)(" ").mkString + k + "\n" + v.toString(depth + 1)
    }.mkString

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
  def setFound(f: (Int, Int) => Int, maxDepth: Int, depth: Int = 0): Int = {
    var i = trie.length - 1
    var last = maxDepth
    while (i >= 0 && depth <= last) {
      if (trie(i) ne null) {
        last = f(depth, i)
        last = trie(i).setFound(f, last, depth + 1)
      }
      i -= 1
    }
    last
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: Seq[Int], depth: Int = 0): ArrayTrie = {
    if (modified eq Nil) this
    else {
      var newTrie: Array[ArrayTrie] = null
      var i = trie.length - 1
      var newSize = 0

      if (modified.head == depth) {
        // Some change at this level
        while (i >= 0) {
          val currentTrie = trie(i)
          if ((currentTrie ne null) && f(depth, i)) {

            val newSubTrie = currentTrie.filterTrie(f, modified.tail, depth + 1)
            if (newSubTrie ne null) {
              if (newTrie eq null) {
                newTrie = new Array[ArrayTrie](i + 1)
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
            val newSubTrie = currentTrie.filterTrie(f, modified, depth + 1)
            if (newSubTrie ne null) {
              if (newTrie eq null) {
                newTrie = new Array[ArrayTrie](i + 1)
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
      else new ArrayTrie(newTrie, newSize)

    }
  }

  //  private def asStream: Stream[List[Int]] =
  //    trie.toStream flatMap {
  //      case (i, t) => if (t.isEmpty) Stream(List(i)) else t.asStream map (i :: _)
  //    }

  private def listiterator: Iterator[List[Int]] = trie.iterator.zipWithIndex flatMap {
    case (t, i) if (t ne null) => if (t eq ArrayTrie.leaf) Iterator(List(i)) else t.listiterator map (i :: _)
    case _ => Nil
  }

  def iterator = listiterator map (_.toArray)
  //  
  //  private def asIterable: Iterable[List[Int]] = trie flatMap {
  //    case (i, t) => if (t.isEmpty) List(List(i)) else t.asIterable map (i :: _)
  //  } 
  //  
  //  override def toList = asList map (_.toArray)

  def nodes: Int = 1 + values.map(_.nodes).sum

  def tupleString = iterator map { _.mkString(" ") } mkString "|"

}


