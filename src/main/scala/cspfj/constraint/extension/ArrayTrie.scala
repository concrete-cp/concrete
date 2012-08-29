package cspfj.constraint.extension

import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.collection.mutable.HashMap
import cspom.Loggable
import java.util.Arrays

object ArrayTrie {

  val default_size = 2

  def empty = new ArrayTrie(new Array(default_size), 0)

  val leaf = new ArrayTrie(Array(), 1)

  def apply(tuples: Array[Int]*) = tuples.foldLeft(empty)(_ + _)
}

final class ArrayTrie(var trie: Array[ArrayTrie], private var _size: Int) {

  def depth: Int = {
    if (this eq ArrayTrie.leaf) 0
    else {
      1 + values.map(_.depth).max
    }
  }

  def size = _size

  private def values = trie.filter(_ ne null)

  def isEmpty = _size == 0

  def +(t: Array[Int]): ArrayTrie = if (contains(t)) this else this + (t, 0)

  /**
   * Increases the capacity of this instance, if necessary, to ensure that it
   * can hold at least the number of elements specified by the minimum
   * capacity argument.
   *
   * @param minCapacity
   *            the desired minimum capacity
   */
  private def ensureCapacity(minCapacity: Int) {
    val oldCapacity = trie.length;
    if (minCapacity > oldCapacity) {
      val newCapacity = math.max(minCapacity, (oldCapacity * 3) / 2 + 1);
      // minCapacity is usually close to size, so this is a win:
      trie = Arrays.copyOf(trie, newCapacity)
    }
  }

  private def +(tuple: Array[Int], i: Int): ArrayTrie = {
    if (i >= tuple.length) ArrayTrie.leaf
    else {
      val v = tuple(i)
      ensureCapacity(v + 1)

      if (trie(v) eq null) {
        if (i == tuple.length - 1) trie(v) = ArrayTrie.leaf
        else trie(v) = ArrayTrie.empty + (tuple, i + 1)
      } else {
        trie(v) += (tuple, i + 1)
      }
      _size += 1
      this
    }
  }

  def -(t: Array[Int]): ArrayTrie = if (contains(t)) (this - (t, 0)) else this

  private def -(tuple: Array[Int], i: Int): ArrayTrie = {
    if (i < tuple.length) {
      val v = tuple(i)

      val t = trie(v)
      if (t eq ArrayTrie.leaf) {
        trie(v) = null
      } else {
        val newTrie = t - (tuple, i + 1)
        if (newTrie.isEmpty) trie(v) = null else trie(v) = newTrie
      }
      _size -= 1
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

  def filterTrie(f: (Int, Int) => Boolean, modified: Seq[Int], depth: Int = 0): ArrayTrie = {
    if (modified eq Nil) this
    else {
      var newTrie: Array[ArrayTrie] = null
      var i = trie.length - 1
      var newSize = 0

      val nextMod = if (modified.head == depth) modified.tail else modified

      if (nextMod eq modified) {
        // No change at this level
        while (i >= 0) {
          val currentTrie = trie(i)
          if (currentTrie ne null) {
            val newSubTrie = currentTrie.filterTrie(f, nextMod, depth + 1)
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
        // Some change at this level
        while (i >= 0) {
          val currentTrie = trie(i)
          if ((currentTrie ne null) && f(depth, i)) {

            val newSubTrie = currentTrie.filterTrie(f, nextMod, depth + 1)
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

  //  /**
  //   * This method returns a copy of this extension with permuted tuples. New
  //   * order of tuples is given as an argument.
  //   *
  //   * <p>
  //   * For example, a ternary extension 1 2 3|1 3 4|2 4 5 will be reversed to 1
  //   * 3 2|1 4 3|2 5 4 by a call to reverse(0, 2, 1).
  //   * </p>
  //   *
  //   * @param newOrder
  //   *            new order of the extension.
  //   * @return a reversed copy of the extension.
  //   */
  //  def permute(newOrder: Seq[Int]) = ArrayTrie(toList map { t => newOrder.map(t(_)).toArray }: _*)

  def tupleString = iterator map { _.mkString(" ") } mkString "|"

}