package concrete

import cspom.util.BitVector
import scala.collection.SortedSet
import concrete.util.Interval
import cspom.util.RangeSet
import cspom.util.Infinitable
import cspom.util.Finite
import cspom.util.FiniteIntInterval

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
  def median = throw new NoSuchElementException
  def isAssigned = throw new UnsupportedOperationException
  def shift(o: Int) = EmptyIntDomain
  def disjoint(d: Domain) = true

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
      assert(s == bv.cardinality)
      if (ub - lb == s - 1) {
        new IntervalDomain(offset + lb, offset + ub)
      } else {
        if (lb >= BitVector.WORD_SIZE) {
          new BitVectorDomain(offset + lb, bv.shift(-lb), s)
        } else {
          new BitVectorDomain(offset, bv, s)
        }
      }
    }
  }

  def apply(d: RangeSet[Infinitable]): IntDomain = {
    val cspom.util.Interval(Finite(offset), Finite(ub)) = d.span

    offset - ub - 1 match {
      case 0 => EmptyIntDomain
      case 1 => Singleton(offset)
      case _ =>
        val finite = d.ranges.toSeq.map {
          case FiniteIntInterval(l, u) => (l - offset, u - offset)
        }
        val bv = BitVector.fromIntervals(finite)
        val s = finite.map { case (l, u) => u - l + 1 }.sum
        ofBitVector(offset, bv, s)
    }
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
  def ofSeq(d: Int*): IntDomain = {
    val offset = d.min
    val bv = BitVector(d.view.map(_ - offset))
    // d may contain the same element twice
    ofBitVector(offset, bv, bv.cardinality)
  }

}

abstract class IntDomain extends Domain {

  def assign(value: Int) = {
    if (present(value)) Singleton(value)
    else EmptyIntDomain
  }

  def |(value: Int): IntDomain

  def removeItv(from: Int, to: Int) = {
    val removed = removeFrom(from) | removeTo(to)
    if (removed.size == size) this else removed
  }

}
