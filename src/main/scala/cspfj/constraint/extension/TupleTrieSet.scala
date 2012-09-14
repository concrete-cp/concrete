package cspfj.constraint.extension;

import scala.annotation.tailrec
import cspom.extension.HashTrie

final class TupleTrieSet(
  private var _trie: ArrayTrie,
  val initialContent: Boolean)
  extends Matrix with Iterable[Array[Int]] {

  def this(initialContent: Boolean) = this(ArrayTrie.empty, initialContent)

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
  
  def arrayTrie = _trie

  override def isEmpty = trie.isEmpty && !initialContent;

  override def size = trie.size

  def iterator = trie.iterator

  //def mutableIterator = tupleSet.iterator

  override def toString =
    (if (initialContent) "nogoods: " else "goods: ") + trie.toString

}
