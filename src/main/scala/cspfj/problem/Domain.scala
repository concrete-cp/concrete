package cspfj.problem;

import cspfj.util.BitVector;

trait Domain {
  def next(i: Int): Int

  def prev(i: Int): Int

  def lastAbsent: Int

  def prevAbsent(i: Int): Int

  def index(value: Int): Int

  def value(index: Int): Int

  def first: Int

  def last: Int

  def isEmpty = first >= 0

  def maxSize = allValues.size

  def present(index: Int): Boolean

  def setSingle(index: Int)

  def remove(index: Int)

  def size: Int

  /**
   * @param lb
   * @return Removes all indexes starting from given lower bound.
   */
  def removeFrom(lb: Int): Int

  /**
   * @param ub
   * @return Removes all indexes up to given upper bound.
   */
  def removeTo(ub: Int): Int

  def setLevel(level: Int)

  def restoreLevel(level: Int)

  def reset() { restoreLevel(0) }

  def getAtLevel(level: Int): BitVector

  def firstValue = value(first)

  def lastValue = value(last)

  def allValues: Array[Int]

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

  def getBitVector: BitVector

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
    var index = last

    def hasNext = index >= 0

    def next = {
      val current = index
      index = Domain.this.prev(index)
      current
    }
  }

  def values = indices map value

  def valueInterval = Interval(firstValue, lastValue)

  def intersectVal(i: Interval) = {
    val lb = closestLt(i.lb)
    val ub = closestGt(i.ub)
    (if (lb >= 0) removeTo(lb) else 0) + (if (ub >= 0) removeFrom(ub) else 0)
  }

}
