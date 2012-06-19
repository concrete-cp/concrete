package cspfj.constraint.extension

import scala.annotation.tailrec
import scala.collection.mutable.HashMap

object Trie {
  var cache = new HashMap[Int, Trie]()

  def empty(depth: Int) = cache.getOrElseUpdate(depth, new Trie(ListMap.empty, 0, depth))

  def apply(tuples: Array[Int]*) = {
    if (tuples.isEmpty) empty(0)
    else {
      val size = tuples.head.length
      tuples.foldLeft(empty(size))(_ + _)
    }
  }
}

final class Trie(val trie: ListMap[Int, Trie], override val size: Int, val maxDepth: Int) extends Iterable[Array[Int]] {

  def get(k: Int) = trie.get(k)

  override def isEmpty = trie.isEmpty

  def +(tuple: Array[Int], i: Int = 0): Trie = {
    assert(tuple.length - i == maxDepth)
    if (i >= tuple.length) this
    else new Trie(trie + (tuple(i) -> (trie.getOrElse(tuple(i), Trie.empty(maxDepth - 1)) + (tuple, i + 1))), size + 1, maxDepth)
  }

  def -(tuple: Array[Int], i: Int = 0): Trie = {
    if (i >= tuple.length) this
    else get(tuple(i)) match {
      case None => this
      case Some(t) => {
        val newTrie = t - (tuple, i + 1)
        if (newTrie.isEmpty) new Trie(trie - tuple(i), size - 1, maxDepth)
        else if (newTrie.size < t.size) new Trie(trie + (tuple(i) -> newTrie), size - 1, maxDepth)
        else this
      }
    }

  }

  @tailrec
  final def contains(tuple: Array[Int], i: Int = 0): Boolean = {
    if (i >= tuple.size) true
    else get(tuple(i)) match {
      case None => false
      case Some(t) => t.contains(tuple, i + 1)
    }
  }

  override def equals(o: Any): Boolean = o match {
    case t: Trie => trie == t.trie
    case _ => false
  }

  override def toString = size + " elements\n" + toString(0)

  private def toString(depth: Int): String =
    trie.map {
      case (k: Int, v: Trie) =>
        List.fill(depth)(" ").mkString + k + "\n" + v.toString(depth + 1)
    }.mkString

  def foreachTrie(f: (Int, Int) => Unit, depth: Int = 0) {
    for ((k, v) <- trie) {
      f(depth, k)
      v.foreachTrie(f, depth + 1)
    }
  }

  def filterTrie(f: (Int, Int) => Boolean, depth: Int = 0): Trie = {
    if (isEmpty) this
    else {
      val m: ListMap[Int, Trie] = trie.filterKeys {
        f(depth, _)
      } mapValues {
        _.filterTrie(f, depth + 1)
      }

      val n: ListMap[Int, Trie] =
        if (depth < maxDepth) {
          m.filter { case (_, v: Trie) => !v.isEmpty }
        } else m

      new Trie(n, n.foldLeft(0)((acc, e) => acc + math.max(1, e._2.size)), maxDepth)
    }
  }
  
  def iterator = throw new UnsupportedOperationException
}