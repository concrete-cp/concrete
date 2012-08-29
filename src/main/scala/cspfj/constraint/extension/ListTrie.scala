package cspfj.constraint.extension

import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.collection.mutable.HashMap
import cspom.Loggable
import java.util.Arrays

object ListTrie {

  val empty = new ListTrie(Nil, 0)

  val leaf = new ListTrie(Nil, 1)

  def apply(tuples: Array[Int]*) = tuples.foldLeft(empty)(_ + _)

}

final class ListTrie(var trie: List[(Int, ListTrie)], private var _size: Int) {

  def depth: Int = {
    if (this eq ListTrie.leaf) 0
    else {
      1 + values.map(_._2.depth).max
    }
  }

  def size = _size

  private def values = trie.filter(_ ne null)

  def isEmpty = _size == 0

  def +(t: Array[Int]): ListTrie = if (contains(t)) this else this + (t, 0)

  private def findAndRemove(i: Int, l: List[(Int, ListTrie)]): (Option[ListTrie], List[(Int, ListTrie)]) =
    if (l eq Nil) (None, Nil)
    else if (l.head._1 == i) (Some(l.head._2), l.tail)
    else {
      val o = findAndRemove(i, l.tail)
      (o._1, l.head :: o._2)
    }

  private def +(tuple: Array[Int], i: Int): ListTrie = {
    if (i >= tuple.length) ListTrie.leaf
    else {
      val v = tuple(i)

      val (subTrie, remainTrie) = findAndRemove(v, trie)

      if (subTrie.isEmpty) {
        if (i == tuple.length - 1) trie ::= (v, ListTrie.leaf)
        else trie ::= (v, ListTrie.empty + (tuple, i + 1))
      } else {
        trie = (v, (subTrie.get + (tuple, i + 1))) :: remainTrie
      }
      _size += 1
      this
    }
  }

  def -(t: Array[Int]): ListTrie = if (contains(t)) (this - (t, 0)) else this

  private def -(tuple: Array[Int], i: Int): ListTrie = {
    if (i < tuple.length) {
      val v = tuple(i)

      val (subTrie, remainTrie) = findAndRemove(v, trie)

      if (subTrie.get eq ListTrie.leaf) {
        trie = remainTrie
      } else {
        val newTrie = subTrie.get - (tuple, i + 1)
        if (newTrie.isEmpty) trie = remainTrie else trie = (v, newTrie) :: remainTrie
      }
      _size -= 1
    }
    this
  }

  def contains(tuple: Array[Int]): Boolean = contains(tuple, 0)

  @tailrec
  private def contains(tuple: Array[Int], i: Int): Boolean = {
    if (i >= tuple.length) true
    else trie.find(_._1 == tuple(i)) match {
      case None => false
      case Some(t) => t._2.contains(tuple, i + 1)
    }

  }

  override def equals(o: Any): Boolean = o match {
    case t: ListTrie => trie sameElements t.trie
    case _ => false
  }

  override lazy val hashCode = trie.toSeq.hashCode

  override def toString = nodes + " nodes representing " + size + " tuples" + toString(0)

  private def toString(depth: Int): String =
    trie.map {
      case (k: Int, v: ListTrie) =>
        List.fill(depth)(" ").mkString + k + "\n" + v.toString(depth + 1)
    }.mkString

  def foreachTrie(f: (Int, Int) => Unit, depth: Int = 0) {
    for (t <- trie) {
      f(depth, t._1)
      t._2.foreachTrie(f, depth + 1)
    }
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: Seq[Int], depth: Int = 0): ListTrie = {
    if (modified eq Nil) this
    else {

      val nextMod = if (modified.head == depth) modified.tail else modified

      val newTrie1 = if (nextMod ne modified) trie.filter(t => f(depth, t._1)) else trie

      val newTrie2 = newTrie1 map (t => (t._1, t._2.filterTrie(f, nextMod, depth + 1))) filter (t => t._2.size > 0)

      if (newTrie2 eq Nil) ListTrie.empty
      else {
        val newSize = newTrie2.foldLeft(0)((acc, t) => acc + t._2.size)
        if (size == newSize) this
        else new ListTrie(newTrie2, newSize)
      }

    }
  }

  //  private def asStream: Stream[List[Int]] =
  //    trie.toStream flatMap {
  //      case (i, t) => if (t.isEmpty) Stream(List(i)) else t.asStream map (i :: _)
  //    }

  private def listiterator: Iterator[List[Int]] = (trie.sortBy(_._1).iterator.flatMap {
    case (i, t) => if (t eq ListTrie.leaf) Iterator(List(i)) else t.listiterator map (i :: _)
  })

  def iterator = listiterator map (_.toArray)
  //  
  //  private def asIterable: Iterable[List[Int]] = trie flatMap {
  //    case (i, t) => if (t.isEmpty) List(List(i)) else t.asIterable map (i :: _)
  //  } 
  //  
  //  override def toList = asList map (_.toArray)

  def nodes: Int = 1 + values.map(_._2.nodes).sum

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
  //  def permute(newOrder: Seq[Int]) = ListTrie(toList map { t => newOrder.map(t(_)).toArray }: _*)

  def tupleString = iterator map { _.mkString(" ") } mkString "|"

}