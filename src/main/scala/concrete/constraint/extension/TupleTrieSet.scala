package concrete.constraint.extension;

import scala.annotation.tailrec

final class TupleTrieSet(
  private var _trie: Relation,
  val initialContent: Boolean)
    extends Matrix with Iterable[Array[Int]] {

  def this(initialContent: Boolean) = this(new MDDRelation(), initialContent)

  def reduceable = _trie

  def copy = new TupleTrieSet(_trie.copy, initialContent)

  def check(tuple: Array[Int]) =
    _trie.contains(tuple) ^ initialContent;

  def set(tuple: Seq[Int], status: Boolean) {
    if (status == initialContent) {
      _trie -= tuple
    } else {
      _trie += tuple
    }
  }

  //  override def setAll(tuples: Iterable[Array[Int]], status: Boolean) {
  //    if (status == initialContent) {
  //      _trie --= tuples
  //    } else
  //      _trie ++= tuples
  //  }

  def allowed() = {
    require(initialContent == false)
    iterator
  }

  override def isEmpty = _trie.isEmpty && !initialContent;

  override def size = _trie.size

  def iterator = _trie.iterator

  //def mutableIterator = tupleSet.iterator

  override def toString =
    (if (initialContent) "nogoods: " else "goods: ") + _trie.toString

}
