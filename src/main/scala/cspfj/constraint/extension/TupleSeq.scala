package cspfj.constraint.extension;

import scala.annotation.tailrec

final class TupleSeq(private var content: Seq[Array[Int]])
  extends Matrix with Iterable[Array[Int]] {

  def this() = this(Nil)

  override def copy = new TupleSeq(content)

  /**
   * Too slow, avoid :
   * content.exists(_ sameElements tuple);
   */
  override def check(tuple: Array[Int]) = throw new UnsupportedOperationException

  override def set(tuple: Array[Int], status: Boolean) {
    assume(status == true)
    content :+= tuple
  }

  override def isEmpty = content.isEmpty

  override def size = content.size

  def iterator = content.iterator

  //def mutableIterator = tupleSet.iterator

  override def toString = "goods: " + content.toString

}
