package concrete

import cspom.util.BitVector
import scala.collection.SortedSet
import concrete.util.Interval

object EmptyIntDomain extends IntDomain {
  def length = 0
  override def head = throw new NoSuchElementException
  override def last = throw new NoSuchElementException
  def next(i: Int) = throw new NoSuchElementException
  def prev(i: Int) = throw new NoSuchElementException
  def prevOrEq(i: Int) = throw new NoSuchElementException
  def nextOrEq(i: Int) = throw new NoSuchElementException
  def present(i: Int) = false
  def remove(i: Int) = this
  def removeFrom(lb: Int) = this
  override def removeAfter(lb: Int) = this
  def removeTo(ub: Int) = this
  override def removeUntil(ub: Int) = this
  override def filter(f: Int => Boolean) = this
  def filterBounds(f: Int => Boolean) = this
  override def toString = "[]"
  def convex = throw new IllegalStateException
  override def isEmpty = true
  override def iterator = Iterator.empty
  def apply(i: Int) = throw new NoSuchElementException
  def toBitVector(offset: Int) = BitVector.empty
  def span = throw new NoSuchElementException
  def singleValue = throw new NoSuchElementException
  def &(d: Domain): Domain = EmptyIntDomain
  def |(d: Domain): Domain = d
  def |(v: Int): IntDomain = Singleton(v)
  def &(lb: Int, ub: Int): Domain = EmptyIntDomain
}

object IntDomain {
  def ofInterval(lb: Int, ub: Int): IntDomain =
    if (lb > ub) { EmptyIntDomain }
    else if (ub == lb) { Singleton(lb) }
    else { new IntervalDomain(lb, ub) }

  def ofInterval(i: Interval): IntDomain = i.size match {
    case 0 => EmptyIntDomain
    case 1 => Singleton(i.lb)
    case _ => new IntervalDomain(i)

  }

  def ofBitVector(offset: Int, bv: BitVector, s: Int): IntDomain = s match {
    case 0 => EmptyIntDomain
    case 1 => Singleton(offset + bv.nextSetBit(0))
    case s: Int => {
      val lb = bv.nextSetBit(0)
      val ub = bv.lastSetBit
      if (ub - lb == s - 1) {
        new IntervalDomain(offset + lb, offset + ub)
      } else {
        new BitVectorDomain(offset, bv, s)
      }
    }
  }

  def apply(d: SortedSet[Int]): IntDomain = {
    val offset = d.head
    ofBitVector(offset, BitVector(d.view.map(_ - offset)), d.size)
  }

  def apply(r: Range): IntDomain =
    if (r.step == 1) {
      ofInterval(r.start, r.last)
    } else {
      val offset = r.start
      ofBitVector(offset, BitVector(r.map(_ - offset)), r.size)
    }

  def apply(i: Interval): IntDomain = ofInterval(i.lb, i.ub)

  @annotation.varargs
  def ofSeq(d: Int*): IntDomain = apply(SortedSet(d: _*))

}

abstract class IntDomain extends Domain {

  def assign(value: Int) = {
    if (present(value)) Singleton(value)
    else EmptyIntDomain
  }

  def |(value: Int): IntDomain

}
