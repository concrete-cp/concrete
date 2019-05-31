package concrete

import bitvectors.BitVector
import concrete.util.Interval
import cspom.Statistic

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.{SortedSetLike, mutable}

object Domain {
  @Statistic
  var checks = 0L

  def searchSpace(s: Seq[Domain]): Double = {
    s.foldLeft(1.0)(_ * _.size)
  }
}

class DomainBuilder extends mutable.Builder[Int, Domain] {
  private val set = TreeSet.newBuilder[Int]

  override def +=(elem: Int): DomainBuilder.this.type = {
    set += elem
    this
  }

  override def clear(): Unit = set.clear

  override def result(): Domain = {
    IntDomain.ofTreeSet(set.result())
  }
}

abstract class Domain extends SortedSet[Int] with SortedSetLike[Int, Domain] {
  def ordering: Ordering[Int] = Ordering.Int

  @deprecated
  override def apply(i: Int): Boolean = contains(i)

  override def newBuilder: mutable.Builder[Int, Domain] = new DomainBuilder

  override def empty: Domain = EmptyIntDomain

  def next(i: Int): Int

  def nextOption(i: Int): Option[Int] = {
    if (i >= last) None else Some(next(i))
  }

  def prev(i: Int): Int

  def prevOption(i: Int): Option[Int] = {
    if (i <= head) None else Some(prev(i))
  }

  def median: Int

  def +(elem: Int): Domain = throw new UnsupportedOperationException

  def assign(value: Int): Domain

  def isAssigned: Boolean

  /**
    * @param lb
    * @return Removes all indexes starting from given lower bound.
    */
  def removeFrom(lb: Int): Domain

  def removeAfter(lb: Int): Domain

  override def from(from: Int): Domain = removeUntil(from)

  override def to(to: Int): Domain = removeAfter(to)

  override def until(until: Int): Domain = removeFrom(until)

  override def range(from: Int, until: Int): Domain = this & (from, until - 1)

  /**
    * @param ub
    * @return Removes all indexes up to given upper bound.
    */
  def removeTo(ub: Int): Domain

  def removeUntil(ub: Int): Domain

  def removeItv(from: Int, to: Int): Domain

  def span: Interval

  def spanFrom(from: Int): Option[Interval] = spanSlice(from = Some(from))
  def spanTo(to: Int): Option[Interval] = spanSlice(to = Some(to))

  def spanSlice(from: Option[Int] = None, to: Option[Int] = None): Option[Interval] = {
    for {
      lb <- from.map(f => nextOption(f - 1)).getOrElse(Some(head))
      ub <- to.map(t => prevOption(t + 1)).getOrElse(Some(last))
      i <- Interval.option(lb, ub)
    } yield {
      i
    }
  }

  def singleValue: Int

  def &(a: Int, b: Int): Domain

  def &(i: Interval): Domain = this & (i.lb, i.ub)

  def &(d: Domain): Domain

  def |(d: Domain): Domain

  def --(d: Domain): Domain = d match {
    case EmptyIntDomain | BooleanDomain.EMPTY => this
    case s@(_: Singleton | BooleanDomain.TRUE | BooleanDomain.FALSE) => this - s.head
    case i: IntervalDomain => removeItv(i.head, i.last)
    case _ if this disjoint d => this
    case _ => this.filterNot(d)
  }

  def disjoint(d: Domain): Boolean

  def rangeImpl(from: Option[Int], to: Option[Int]): Domain = {
    val f = from.map(removeUntil).getOrElse(this)
    to.map(f.removeAfter).getOrElse(f)
  }

  def subsetOf(d: Domain): Boolean

  def convex: Boolean

  def toBitVector(offset: Int): BitVector

  override def equals(o: Any): Boolean = this eq o.asInstanceOf[AnyRef]

  def filterBounds(f: Int => Boolean): Domain

  def shift(o: Int): Domain
}
