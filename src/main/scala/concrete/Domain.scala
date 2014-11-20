package concrete

import scala.collection.AbstractSeq
import scala.collection.TraversableLike
import concrete.util.BitVector
import concrete.util.Interval
import cspom.Statistic
import scala.collection.IterableLike
import scala.collection.mutable.Builder

object Domain {
  @Statistic
  var checks = 0L
}

abstract class Domain extends AbstractSeq[Int] with IterableLike[Int, Domain] {
  override def newBuilder: Builder[Int, Domain] = ???
  def next(i: Int): Int

  def prev(i: Int): Int

  def present(value: Int): Boolean
  
  def apply(i:Int) = throw new UnsupportedOperationException

  override def contains[A >: Int](value: A) = present(value.asInstanceOf[Int])

  override def indices = throw new AssertionError

  def remove(value: Int): Domain

  /**
   * @param lb
   * @return Removes all indexes starting from given lower bound.
   */
  def removeFrom(lb: Int): Domain

  def removeAfter(lb: Int): Domain = if (lb >= last) this else removeFrom(next(lb))

  /**
   * @param ub
   * @return Removes all indexes up to given upper bound.
   */
  def removeTo(ub: Int): Domain

  def removeUntil(ub: Int): Domain = {
    if (ub <= head) this else removeTo(prev(ub))
  }

  /**
   * @param value
   * @return the index of the closest value lower or equal to the given value.
   */
  def prevOrEq(value: Int): Int

  /**
   * @param value
   * @return the index of the closest value greater or equal to the given
   *         value.
   */
  def nextOrEq(value: Int): Int

  def span: Interval = Interval(head, last)

  def &(a: Int, b: Int): Domain = removeUntil(a).removeAfter(b)
  def &(i: Interval): Domain = this & (i.lb, i.ub)

  final def disjoint(d: Domain): Boolean = {
    last < d.head || head > d.last || forall(v => !d.present(v))
  }

  def intersects(bv: BitVector): Int
  def intersects(bv: BitVector, part: Int): Boolean
  def bound: Boolean

  def toBitVector: BitVector
}
