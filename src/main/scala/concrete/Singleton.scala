package concrete

import concrete.util.BitVector
import scala.collection.mutable.HashMap
import concrete.util.Interval
import com.typesafe.scalalogging.LazyLogging

object Singleton {
  val cache = new HashMap[Int, Singleton]
  def apply(v: Int) = cache.getOrElseUpdate(v, new Singleton(v))
}

final class Singleton(val singleValue: Int) extends IntDomain with LazyLogging {

  def length = 1

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

  override def toString() = s"[$singleValue]"

  def subsetOf(d: IntDomain) = d.present(singleValue)

  lazy val toBitVector = BitVector.empty + singleValue

  //  override def intersects(bv: BitVector) = {
  //    val part = value >> 6
  //    if (bv(value)) part else -1
  //  }

  var requestedOffset: Int = _
  var requestedBV: BitVector = null

  def bitVector(offset: Int) =
    if (offset == 0)
      toBitVector
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
  def bound = true
  override def isEmpty = false

  override def assign(v: Int) = {
    if (v == singleValue) this else EmptyIntDomain
  }
  
  def iterator = Iterator.single(singleValue)
}
