package concrete

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.util.Interval

import scala.collection.mutable.HashMap

object Singleton {
  val cache = new HashMap[Int, Singleton]

  def apply(v: Int) = cache.getOrElseUpdate(v, new Singleton(v))
}

final class Singleton private(val singleValue: Int) extends IntDomain with LazyLogging {

  lazy val bitVector = BitVector.empty + singleValue
  val span = Interval(singleValue, singleValue)
  var requestedOffset: Int = _
  var requestedBV: BitVector = _

  def size = 1

  override def head = singleValue

  override def last = singleValue

  def next(i: Int) = if (i < singleValue) singleValue else throw new NoSuchElementException

  def prev(i: Int) = if (i > singleValue) singleValue else throw new NoSuchElementException

  def prevOrEq(i: Int): Int =
    if (i >= singleValue) {
      singleValue
    }
    else {
      throw new NoSuchElementException
    }

  def nextOrEq(i: Int): Int =
    if (i <= singleValue) {
      singleValue
    }
    else {
      throw new NoSuchElementException
    }

  /**
    * @param i
    * index to test
    * @return true iff index is present
    */
  def present(i: Int) = i == singleValue

  def remove(i: Int) = {
    if (singleValue == i) EmptyIntDomain else this
  }

  def removeFrom(lb: Int) =
    if (lb > singleValue) {
      this
    }
    else {
      EmptyIntDomain
    }

  def removeAfter(lb: Int) = {
    if (lb >= singleValue) {
      this
    }
    else {
      EmptyIntDomain
    }
  }

  def removeTo(ub: Int) =
    if (ub < singleValue) {
      this
    }
    else {
      EmptyIntDomain
    }

  def removeUntil(ub: Int) = {
    if (ub <= singleValue) {
      this
    }
    else {
      EmptyIntDomain
    }
  }

  def filterBounds(f: Int => Boolean) = filter(f)

  override def filter(f: Int => Boolean) =
    if (f(singleValue)) {
      this
    }
    else {
      EmptyIntDomain
    }

  //  override def intersects(bv: BitVector) = {
  //    val part = value >> 6
  //    if (bv(value)) part else -1
  //  }

  override def toString() = s"[$singleValue]"

  def subsetOf(d: IntDomain) = d.present(singleValue)

  def isAssigned = true

  def &(d: Domain): Domain = d match {
    case bd: BooleanDomain => bd & this
    case _ if d.present(singleValue) => this
    case _ => EmptyIntDomain
  }

  def &(lb: Int, ub: Int): Domain = if (lb <= singleValue && singleValue <= ub) this else EmptyIntDomain

  def |(d: Domain): Domain = d match {
    case EmptyIntDomain => this

    case bd: BooleanDomain => bd.as01 | singleValue

    case d: IntDomain => d | singleValue
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
