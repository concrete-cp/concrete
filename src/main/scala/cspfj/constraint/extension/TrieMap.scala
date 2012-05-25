package cspfj.constraint.extension

import scala.collection.immutable.IntMap
import scala.annotation.tailrec

object TrieMap {
  var cache: IntMap[TrieMap] = IntMap.empty

  def empty(depth: Int) = cache.getOrElse(depth, {
    val trie = new TrieMap(IntMap.empty, 0, depth)
    cache += depth -> trie
    trie
  })

  def of(tuples: Iterable[Array[Int]]) = {
    if (tuples.isEmpty) empty(0)
    else {
      val size = tuples.head.length
      tuples.foldLeft(empty(size))(_ + _)
    }

  }
}

final class TrieMap(val trie: IntMap[TrieMap], val size: Int, val maxDepth: Int) {

  def +(tuple: List[Int]): TrieMap = {
    assert(tuple.size == maxDepth)
    if (tuple.isEmpty) this
    else new TrieMap(trie + (tuple.head -> (trie.getOrElse(tuple.head, TrieMap.empty(maxDepth - 1)) + tuple.tail)), size + 1, maxDepth)
  }

  def +(tuple: Array[Int], i: Int = 0): TrieMap = {
    assert(i == maxDepth)
    if (i >= tuple.length) this
    else new TrieMap(trie + (tuple(i) -> (trie.getOrElse(tuple(i), TrieMap.empty(maxDepth - 1)) + (tuple, i + 1))), size + 1, maxDepth)
  }

  @tailrec
  def contains(tuple: List[Int]): Boolean = {
    if (tuple.isEmpty) true
    else trie.get(tuple.head) match {
      case None => false
      case Some(t) => t.contains(tuple.tail)
    }
  }

  @tailrec
  def contains(tuple: Array[Int], i: Int = 0): Boolean = {
    if (i >= tuple.size) true
    else trie.get(tuple(i)) match {
      case None => false
      case Some(t) => t.contains(tuple, i + 1)
    }
  }

  override def equals(o: Any): Boolean = o match {
    case t: TrieMap => trie.size == t.trie.size && trie.forall {
      case (k, v) => t.trie(k).equals(v)
    }
    case _ => false
  }

  override def toString = size + " elements\n" + toString(0)

  private def toString(depth: Int): String =
    trie.map {
      case (k: Int, v: TrieMap) =>
        List.fill(depth)(" ").mkString + k + "\n" + v.toString(depth + 1)
    }.mkString

  def filterTrie(f: (Int, Int) => Boolean, depth: Int = 0): TrieMap = {
    if (isEmpty) this
    else {
      val m = trie.filter {
        case (k: Int, _) => f(depth, k)
      } map {
        case (k: Int, v: TrieMap) => k -> v.filterTrie(f, depth + 1)
      }

      val n =
        if (depth < maxDepth) {
          m.filter { case (_, v: TrieMap) => !v.isEmpty }
        } else m

      new TrieMap(n, n.foldLeft(0)((acc, e) => acc + math.max(1, e._2.size)), maxDepth)
    }
  }

  def foreachTrie(f: (Int, Int) => Unit, depth: Int = 0) {
    if (!isEmpty) {
      for ((k, v) <- trie) {
        f(depth, k)
        v.foreachTrie(f, depth + 1)
      }
    }
  }

  def isEmpty = trie.isEmpty
}