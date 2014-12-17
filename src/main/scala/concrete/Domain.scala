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

  override def contains[A >: Int](value: A) = present(value.asInstanceOf[Int])

  override def indices = throw new AssertionError

  def remove(value: Int): Domain

  def assign(value: Int): Domain

  /**
   * @param lb
   * @return Removes all indexes starting from given lower bound.
   */
  def removeFrom(lb: Int): Domain

  def removeAfter(lb: Int): Domain
  /**
   * @param ub
   * @return Removes all indexes up to given upper bound.
   */
  def removeTo(ub: Int): Domain

  def removeUntil(ub: Int): Domain

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

  def span: Interval // = Interval(head, last)

  def singleValue: Int

  def &(a: Int, b: Int): Domain = removeUntil(a).removeAfter(b)
  def &(i: Interval): Domain = this & (i.lb, i.ub)

  final def disjoint(d: Domain): Boolean = {
    last < d.head || head > d.last || forall(v => !d.present(v))
  }

  def subsetOf(d: Domain): Boolean = {
    head >= d.head && last <= d.last && (d.bound || forall(d.present))
  }

  //def intersects(bv: BitVector): Int = bv.intersects(toBitVector)
  //def intersects(bv: BitVector, part: Int): Boolean = bv.intersects(toBitVector, part)

  def bound: Boolean

  def bitVector(offset: Int): BitVector

  override final def size = length

}
