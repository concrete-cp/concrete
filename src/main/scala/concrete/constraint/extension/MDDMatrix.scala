package concrete.constraint.extension

import mdd.{MDD, MDD0}

final class MDDMatrix(
                       var mdd: MDD,
                       val initialContent: Boolean)
  extends Matrix {

  def this(initialContent: Boolean) = this(MDD0, initialContent)

  // def copy = new TupleTrieSet(relation.copy, initialContent)

  def check(tuple: Array[Int]): Boolean = mdd.contains(tuple) ^ initialContent

  def set(tuple: Array[Int], status: Boolean): Unit = {
    ???
  }

  def allowed: Iterator[Array[Int]] = {
    require(initialContent == false)
    mdd.iterator.map(_.toArray)
  }

  override def isEmpty: Boolean = mdd.isEmpty && !initialContent

  override def size: Int = mdd.lambda().toInt

  //def mutableIterator = tupleSet.iterator

  override def toString: String =
    (if (initialContent) "nogoods: " else "goods: ") + mdd.toString

}
