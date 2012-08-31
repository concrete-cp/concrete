package cspfj.constraint.extension

import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.collection.mutable.HashMap
import cspom.Loggable
import java.util.Arrays
import cspfj.util.BitVector

object ListTrie {

  val empty = new ListTrie(Nil, 0)

  val leaf = new ListTrie(Nil, 1)

  def apply(tuples: Array[Int]*) = tuples.foldLeft(empty)(_ + _)

  def findAndRemove[A](f: A => Boolean, l: List[A]): (Option[A], List[A]) = {
    if (l eq Nil) (None, Nil)
    else if (f(l.head)) (Some(l.head), l.tail)
    else {
      val o = findAndRemove(f, l.tail)
      (o._1, l.head :: o._2)
    }
  }

  @tailrec
  def updateOrAdd[A](l: List[A], q: A => Boolean, f: Option[A] => A, out: List[A] = Nil): List[A] = {
    if (l eq Nil) f(None) :: out
    else if (q(l.head)) f(Some(l.head)) :: out ::: l.tail
    else updateOrAdd(l.tail, q, f, l.head :: out)
  }

}

final class ListTrie(val trie: List[(Int, ListTrie)], private val _size: Int) {

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

      new ListTrie(ListTrie.updateOrAdd[(Int, ListTrie)](
        trie,
        { e: (Int, ListTrie) => e._1 == v }, {
          case Some(o) => (v, o._2 + (tuple, i + 1))
          case None =>
            if (i == tuple.length - 1) (v, ListTrie.leaf)
            else (v, ListTrie.empty + (tuple, i + 1))
        }),
        _size + 1)
    }
  }

  def -(t: Array[Int]): ListTrie = if (contains(t)) (this - (t, 0)) else this

  private def -(tuple: Array[Int], i: Int): ListTrie = {
    if (i < tuple.length) {
      val v = tuple(i)

      val (subTrie, remainTrie) = findAndRemove(v, trie)

      if (subTrie.get eq ListTrie.leaf) {
        new ListTrie(remainTrie, _size - 1)
      } else {
        val newTrie = subTrie.get - (tuple, i + 1)
        if (newTrie.isEmpty) new ListTrie(remainTrie, _size - 1)
        else new ListTrie((v, newTrie) :: remainTrie, _size - 1)
      }
    } else this
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
    case t: ListTrie => listiterator sameElements t.listiterator
    case _ => false
  }

  override lazy val hashCode = trie.toSeq.hashCode

  override def toString = nodes + " nodes representing " + size + " tuples " + listiterator.mkString("\n") //toString(0)

  private def toString(depth: Int): String =
    trie.map {
      case (k: Int, v: ListTrie) =>
        List.fill(depth)(" ").mkString + k + "\n" + v.toString(depth + 1)
    }.mkString

  def foreachTrie(f: (Int, Int) => Unit, depth: Int = 0) {
    var t = trie
    while (t ne Nil) {
      val (v, s) = t.head
      f(depth, v)
      s.foreachTrie(f, depth + 1)
      t = t.tail
    }

  }

  def setFound(f: Array[BitVector], depth: Int = 0) {
    var t = trie
    while (t ne Nil) {
      val (v, s) = t.head
      f(depth).set(v)
      s.setFound(f, depth + 1)
      t = t.tail
    }
  }

  def filterTrie(f: (Int, Int) => Boolean, modified: Seq[Int], depth: Int = 0): ListTrie = {
    if (modified eq Nil) this
    else {

      val nextMod = if (modified.head == depth) modified.tail else modified

      val newTrie1 = if (nextMod ne modified) trie.filter(t => f(depth, t._1)) else trie

      var newTrie2: List[(Int, ListTrie)] = Nil
      var c = newTrie1
      while (c ne Nil) {
        val t = c.head._2.filterTrie(f, nextMod, depth + 1)
        if (t.size > 0) newTrie2 ::= (c.head._1, t)
        c = c.tail
      }

      // val newTrie2 = newTrie1.map(t => (t._1, t._2.filterTrie(f, nextMod, depth + 1))).filter(t => t._2.size > 0)

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