package cspfj.constraint.extension;

import scala.annotation.tailrec
import cspom.extension.HashTrie
import cspfj.Parameter
import cspfj.ParameterManager

final class TupleTrieSet(
  private var _trie: Relation,
  val initialContent: Boolean)
  extends Matrix with Iterable[Array[Int]] {

  def this(initialContent: Boolean) = this(new MDD(), initialContent)

  def reduceable = _trie

  def copy = new TupleTrieSet(_trie.copy, initialContent)

  def check(tuple: Array[Int]) =
    _trie.contains(tuple) ^ initialContent;

  def set(tuple: Array[Int], status: Boolean) {
    if (status == initialContent) {
      _trie -= tuple
    } else {
      _trie += tuple
    }
  }

  override def setAll(tuples: Iterable[Array[Int]], status: Boolean) {
    if (status == initialContent) {
      _trie --= tuples
    } else
      _trie ++= tuples
  }

  def arrayTrie = _trie

  override def isEmpty = _trie.isEmpty && !initialContent;

  override def size = _trie.size

  def iterator = _trie.iterator

  //def mutableIterator = tupleSet.iterator

  override def toString =
    (if (initialContent) "nogoods: " else "goods: ") + _trie.toString

}
