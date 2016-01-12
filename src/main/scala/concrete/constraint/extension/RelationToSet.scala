package concrete.constraint.extension;

import scala.annotation.tailrec

final class TupleTrieSet(
  var relation: Relation,
  val initialContent: Boolean)
    extends Matrix {

  def this(initialContent: Boolean) = this(new MDDRelation(MDD0), initialContent)

  def copy = new TupleTrieSet(relation.copy, initialContent)

  def check(tuple: Array[Int]) =
    relation.contains(tuple) ^ initialContent;

  def set(tuple: Seq[Int], status: Boolean) {
    if (status == initialContent) {
      relation -= tuple
    } else {
      relation += tuple
    }
  }

  //  override def setAll(tuples: Iterable[Array[Int]], status: Boolean) {
  //    if (status == initialContent) {
  //      relation --= tuples
  //    } else
  //      relation ++= tuples
  //  }

  def allowed() = {
    require(initialContent == false)
    relation.iterator
  }

  override def isEmpty = relation.isEmpty && !initialContent;

  override def size = relation.size

  //def mutableIterator = tupleSet.iterator

  override def toString =
    (if (initialContent) "nogoods: " else "goods: ") + relation.toString

}
