package cspfj.constraint.extension

import scala.collection.immutable.IntMap
import scala.annotation.tailrec

object TrieList {
  val empty = new TrieList(Nil, 0)
  def of(tuples: Iterable[Array[Int]]) =
    tuples.foldLeft(empty)(_ + _)
}

final class TrieList(val trie: List[(Int, TrieList)], val size: Int) {

  def update(k: Int, trie: List[(Int, TrieList)], tuple: Array[Int], i: Int): List[(Int, TrieList)] = trie match {
    case Nil => List((k, TrieList.empty + (tuple, i + 1)))
    case h :: t =>
      if (h._1 == k) {
        (k, h._2 + (tuple, i + 1)) :: t
      } else {
        h :: update(k, t, tuple, i)
      }
  }

  def +(tuple: Array[Int], i: Int = 0): TrieList =
    new TrieList(update(tuple(i), trie, tuple, i), size + 1)

  @tailrec
  def contains(tuple: List[Int]): Boolean = {
    if (tuple.isEmpty) true
    else trie.find(_._1 == tuple.head) match {
      case None => false
      case Some(t) => t._2.contains(tuple.tail)
    }
  }

  @tailrec
  def contains(tuple: Array[Int], i: Int = 0): Boolean = {
    if (i >= tuple.length) true
    else trie.find(_._1 == tuple(i)) match {
      case None => false
      case Some(t) => t._2.contains(tuple, i + 1)
    }
  }

  override def equals(o: Any): Boolean = o match {
    case t: TrieList => trie.size == t.trie.size && trie.forall {
      case (k, v) => t.trie(k).equals(v)
    }
    case _ => false
  }

  override def toString = size + " elements\n" + toString(0)

  private def toString(depth: Int): String =
    trie.map {
      case (k: Int, v: TrieList) =>
        List.fill(depth)(" ").mkString + k + "\n" + v.toString(depth + 1)
    }.mkString

  def isEmpty = trie.isEmpty
}