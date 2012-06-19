package cspfj.constraint.extension;

import cspfj.util.IntTupleSet;
import scala.collection.JavaConversions

final class TupleTrieSet(
  private var tupleSet: Trie,
  val initialContent: Boolean)
  extends Matrix with Iterable[Array[Int]] {

  def this(width: Int, initialContent: Boolean) = this(Trie.empty(width), initialContent)

  override def copy = new TupleTrieSet(tupleSet, initialContent)

  override def check(tuple: Array[Int]) =
    tupleSet.contains(tuple) ^ initialContent;

  override def set(tuple: Array[Int], status: Boolean) {
    if (status == initialContent) {
      tupleSet -= tuple
    } else {
      tupleSet += tuple
    }
  }

  override def isEmpty = tupleSet.isEmpty && !initialContent;

  override def size = tupleSet.size

  def iterator = tupleSet.iterator

  //def mutableIterator = tupleSet.iterator

  override def toString =
    (if (initialContent) "nogoods: " else "goods: ") + tupleSet.toString

}
