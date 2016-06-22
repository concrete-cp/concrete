package concrete

import scala.collection.mutable.HashMap

import com.typesafe.scalalogging.LazyLogging

import cspom.util.BitVector
import concrete.util.Interval

object Singleton {
  val cache = new HashMap[Int, Singleton]
  def apply(v: Int) = cache.getOrElseUpdate(v, new Singleton(v))
}

final class Singleton private (val singleValue: Int) extends IntDomain with LazyLogging {

  def size = 1

  override def head = singleValue

  override def last = singleValue

  def next(i: Int) = if (i < singleValue) singleValue else throw new NoSuchElementException

  def prev(i: Int) = if (i > singleValue) singleValue else throw new NoSuchElementException

  def prevOrEq(i: Int): Int =
    if (i >= singleValue) { singleValue }
    else { throw new NoSuchElementException }

  def nextOrEq(i: Int): Int =
    if (i <= singleValue) { singleValue }
    else { throw new NoSuchElementException }

  val span = Interval(singleValue, singleValue)

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(i: Int) = i == singleValue

  def remove(i: Int) = {
    if (singleValue == i) EmptyIntDomain else this
  }

  def removeFrom(lb: Int) =
    if (lb > singleValue) { this }
    else { EmptyIntDomain }

  def removeAfter(lb: Int) = {
    if (lb >= singleValue) { this }
    else { EmptyIntDomain }
  }

  def removeTo(ub: Int) =
    if (ub < singleValue) { this }
    else { EmptyIntDomain }

  def removeUntil(ub: Int) = {
    if (ub <= singleValue) { this }
    else { EmptyIntDomain }
  }

  override def filter(f: Int => Boolean) =
    if (f(singleValue)) { this }
    else { EmptyIntDomain }

  def filterBounds(f: Int => Boolean) = filter(f)

  override def toString() = s"[$singleValue]"

  def subsetOf(d: IntDomain) = d.present(singleValue)

  lazy val bitVector = BitVector.empty + singleValue

  //  override def intersects(bv: BitVector) = {
  //    val part = value >> 6
  //    if (bv(value)) part else -1
  //  }

  var requestedOffset: Int = _
  var requestedBV: BitVector = _

  def isAssigned = true

  def toBitVector(offset: Int) =
    if (offset == 0)
      bitVector
    else if (requestedBV != null && offset == requestedOffset) {
      requestedBV
    } else {
      requestedOffset = offset
      requestedBV = BitVector.empty + (singleValue - offset)
      requestedBV
    }

  def &(d: Domain) = d match {
    case bd: BooleanDomain             => bd & this
    case d if (d.present(singleValue)) => this
    case _                             => EmptyIntDomain
  }

  def &(lb: Int, ub: Int) = if (lb <= singleValue && singleValue <= ub) this else EmptyIntDomain

  def |(d: Domain) = d match {
    case bv: BitVectorDomain => bv | singleValue

    case s: Singleton        => s | singleValue

    case i: IntervalDomain   => i | singleValue

    case EmptyIntDomain      => this

    case bd: BooleanDomain   => bd.as01 | singleValue
  }

  def |(v: Int) = {
    if (v == singleValue) {
      this
    } else {
      val offset = math.min(singleValue, v)
      val union = toBitVector(offset) + (v - offset)
      IntDomain.ofBitVector(offset, union, 2)
    }
  }

  def apply(i: Int) = if (i == 0) singleValue else throw new IndexOutOfBoundsException
  //
  //  override def intersects(bv: BitVector, part: Int) = bv(value)
  def convex = true
  override def isEmpty = false

  def iterator = Iterator.single(singleValue)

  override def foreach[U](f: Int => U): Unit = f(singleValue)

  def median = singleValue

  def shift(o: Int) = if (o == 0) this else
    Singleton(singleValue + o)

  def disjoint(d: Domain) = !d.present(singleValue)
}
