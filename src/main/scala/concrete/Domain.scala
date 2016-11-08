package concrete

import concrete.util.Interval
import cspom.Statistic
import cspom.util.BitVector
import scala.collection.TraversableView

object Domain {
  @Statistic
  var checks = 0L

  def searchSpace(s: Seq[Domain]) = {
    s.foldLeft(1.0)(_ * _.size)
  }
}

abstract class Domain { //extends AbstractSeq[Int] with IterableLike[Int, Domain] {
  //override def newBuilder: Builder[Int, Domain] = ???

  def next(i: Int): Int

  def nextOption(i: Int): Option[Int] = {
    if (i >= last) None else Some(next(i))
  }

  def prev(i: Int): Int

  def prevOption(i: Int): Option[Int] = {
    if (i <= head) None else Some(prev(i))
  }

  def head: Int //= next(0)
  def last: Int

  def foreach[S](f: Int => S): Unit

  def forall(p: Int => Boolean): Boolean = {
    foreach { x =>
      if (!p(x)) { return false }
    }
    true
  }

  def find(p: Int => Boolean): Option[Int] = {
    foreach { x =>
      if (p(x)) return Some(x)
    }
    None
  }

  def exists(p: Int => Boolean): Boolean = {
    foreach { x =>
      if (p(x)) return true
    }
    false
  }

  def filter(p: Int => Boolean): Domain

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def view = new TraversableView[Int, Domain] {
    protected lazy val underlying = Domain.this
    override def foreach[U](f: Int => U) = Domain.this.foreach(f)
    override def isEmpty = Domain.this.isEmpty
  }

  def present(value: Int): Boolean

  def median: Int

  //  override def contains[A >: Int](value: A) = present(value.asInstanceOf[Int])
  //
  //  override def indices = throw new AssertionError

  def remove(value: Int): Domain

  def removeIfPresent(value: Int): Domain = {
    if (present(value)) {
      remove(value)
    } else {
      this
    }
  }

  def assign(value: Int): Domain

  def isAssigned: Boolean

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

  def removeItv(from: Int, to: Int): Domain

  def span: Interval // = Interval(head, last)

  def singleValue: Int

  def &(a: Int, b: Int): Domain // = removeUntil(a).removeAfter(b)
  def &(i: Interval): Domain = this & (i.lb, i.ub)
  def &(d: Domain): Domain
  def |(d: Domain): Domain

  def disjoint(d: Domain): Boolean // = (this & d).isEmpty

  //  = {
  //    last < d.head || head > d.last || forall(v => !d.present(v))
  //  }

  def subsetOf(d: Domain): Boolean = {
    (this | d).size == d.size //head >= d.head && last <= d.last && (d.convex || forall(d.present))
  }

  //def intersects(bv: BitVector): Int = bv.intersects(toBitVector)
  //def intersects(bv: BitVector, part: Int): Boolean = bv.intersects(toBitVector, part)

  def convex: Boolean

  def toBitVector(offset: Int): BitVector

  def size: Int

  override def equals(o: Any) = this eq o.asInstanceOf[AnyRef]

  def filterBounds(f: Int => Boolean): Domain

  def shift(o: Int): Domain

}
