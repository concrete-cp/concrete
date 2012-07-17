package cspfj.constraint.extension;

import scala.annotation.tailrec
import cspom.extension.Trie

final class TupleTrieSet(
  private var _trie: Trie,
  val initialContent: Boolean)
  extends Matrix with Iterable[Array[Int]] {

  def this(width: Int, initialContent: Boolean) = this(Trie.empty(width), initialContent)

  def trie = _trie
  
  override def copy = new TupleTrieSet(trie, initialContent)

  override def check(tuple: Array[Int]) =
    trie.contains(tuple) ^ initialContent;

  override def set(tuple: Array[Int], status: Boolean) {
    if (status == initialContent) {
      _trie -= tuple
    } else {
      _trie += tuple
    }
  }

  override def isEmpty = trie.isEmpty && !initialContent;

  override def size = trie.size

  def iterator = trie.iterator

  //def mutableIterator = tupleSet.iterator

  override def toString =
    (if (initialContent) "nogoods: " else "goods: ") + trie.toString

}
