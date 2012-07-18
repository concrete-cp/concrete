package cspfj

import cspfj.util.BitVector
import cspfj.util.Interval
import cspfj.util.IntSet

final class EmptyDomainException extends UNSATException

object Domain {
  val empty = new EmptyDomainException
}

abstract class Domain {

  def next(i: Int): Int

  def prev(i: Int): Int

  def index(value: Int): Int

  def value(index: Int): Int

  def first: Int

  def last: Int

  def isEmpty = first < 0

  def maxSize: Int //= allValues.size

  def present(index: Int): Boolean

  def presentVal(value: Int) = {
    val i = index(value)
    i >= 0 && present(i)
  }

  def setSingle(index: Int)

  def remove(index: Int)

  def size: Int

  /**
   * @param lb
   * @return Removes all indexes starting from given lower bound.
   */
  def removeFrom(lb: Int): Boolean

  def removeFromVal(lb: Int) = {
    val v = closestGeq(lb)
    if (v < 0) false
    else removeFrom(v)
  }

  /**
   * @param ub
   * @return Removes all indexes up to given upper bound.
   */
  def removeTo(ub: Int): Boolean

  def removeToVal(ub: Int) = {
    val v = closestLeq(ub)
    if (v < 0) false
    else removeTo(closestLeq(ub))
  }

  def filter(f: Int => Boolean): Boolean

  def setLevel(level: Int)

  def restoreLevel(level: Int)

  def currentLevel: Int

  def getAtLevel(level: Int): BitVector

  def firstValue = value(first)

  def lastValue = value(last)

  //def allValues: Array[Int]

  override def equals(o: Any) = o match {
    case d: Domain => values.sameElements(d.values)
    case _ => false
  }
  
   /**
   * @param value
   * @return the index of the closest value strictly lower than the given value.
   */
  def closestLt(value: Int) = {
    val lb = closestLeq(value);
    if (lb >= 0 && this.value(lb) == value) {
      prev(lb)
    } else {
      lb
    }
  }

  /**
   * @param value
   * @return the index of the closest value strictly greater than the given
   *         value.
   */
  def closestGt(value: Int) = {
    val ub = closestGeq(value);
    if (ub >= 0 && this.value(ub) == value) {
      next(ub)
    } else {
      ub
    }
  }

  /**
   * @param value
   * @return the index of the closest value lower or equal to the given value.
   */
  def closestLeq(value: Int): Int

  /**
   * @param value
   * @return the index of the closest value greater or equal to the given
   *         value.
   */
  def closestGeq(value: Int): Int

  def indices: Iterator[Int] = indices(first)

  def indices(from: Int) = new Iterator[Int] {
    var index = from

    override def hasNext = index >= 0

    override def next = {
      val current = index
      index = Domain.this.next(index);
      current
    }
  }

  def indicesR = new Iterator[Int] {
    var index = Domain.this.last

    override def hasNext = index >= 0

    override def next = {
      val current = index
      index = Domain.this.prev(index);
      current
    }
  }

  def values = indices map value

  def valueInterval = Interval(firstValue, lastValue)

  def intersect(a: Int, b: Int) = removeTo(a - 1) | removeFrom(b + 1)

  def intersectVal(a: Int, b: Int): Boolean = {
    var ch = false
    if (a > firstValue) {
      val lb = closestLt(a)
      ch |= lb >= 0 && removeTo(lb)
    }
    if (b < lastValue) {
      val ub = closestGt(b)
      ch |= ub >= 0 && removeFrom(ub)
    }
    ch
  }

  def intersectVal(i: Interval): Boolean = intersectVal(i.lb, i.ub)

  def removeItvVal(a: Int, b: Int): Boolean = {
    filter(i => a <= value(i) && value(i) <= b)
  }

  def removeValInterval(lb: Int, ub: Int) = {

    var i = closestGt(lb)
    val end = closestLt(ub)

    if (i >= 0 && end >= 0)
      while (i <= end) {
        if (present(i)) remove(i)
        i += 1
      }
  }

  def disjoint(d: Domain, i: Int = first): Boolean =
    if (i < 0) true
    else {
      val i2 = d.index(value(i))
      (i2 < 0 || !d.present(i2)) && disjoint(d, next(i))
    }

  def intersects(bv: BitVector): Int
  def intersects(bv: BitVector, part: Int): Boolean
  def bound: Boolean
  def intSet: IntSet

}
