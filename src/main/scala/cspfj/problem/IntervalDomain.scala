package cspfj.problem;

import cspfj.util.BitVector;
import java.util.Arrays

final class IntervalDomain(
  private val offset: Int,
  private val initDomain: Interval,
  private var currentDomain: Interval,
  /**
   * History entry is (level, domain, domain size)
   */
  private var history: List[(Int, Interval)]) extends Domain {

  override def size = currentDomain.size

  private var currentLevel = 0;

  private var removed = false

  def this(domain: IntervalDomain) =
    this(domain.offset, domain.initDomain, domain.currentDomain, domain.history)

  def this(lb: Int, ub: Int) = this(lb, Interval(0, ub - lb), Interval(0, ub - lb), Nil)

  override def first = currentDomain.lb

  override def last = currentDomain.ub

  override def lastAbsent = throw new UnsupportedOperationException

  override def next(i: Int) = if (i >= currentDomain.ub) -1 else i + 1

  override def prev(i: Int) = if (i <= currentDomain.lb) -1 else i - 1

  override def prevAbsent(i: Int) = throw new UnsupportedOperationException

  /**
   * @param value
   *            the value we seek the index for
   * @return the index of the given value or -1 if it could not be found
   */
  override def index(value: Int) = value - offset

  private def closestLeq(value: Int, lb: Int, ub: Int): Int = {
    if (this.value(ub) <= value) {
      ub
    } else {
      val test = (ub + lb) / 2
      if (this.value(test) > value) {
        closestLeq(value, lb, test - 1)
      } else {
        closestLeq(value, test + 1, ub)
      }
    }
  }

  override def closestLeq(value: Int): Int =
    if (this.value(first) > value) -1 else closestLeq(value, first, last)

  private def closestGeq(value: Int, lb: Int, ub: Int): Int = {
    if (this.value(lb) >= value) {
      lb
    } else {
      val test = (ub + lb) / 2;
      if (this.value(test) >= value) {
        closestGeq(value, lb, test - 1)
      } else {
        closestGeq(value, test + 1, ub)
      }
    }
  }

  override def closestGeq(value: Int): Int =
    if (this.value(last) < value) -1 else closestGeq(value, first, last)

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  override def present(index: Int) = currentDomain.in(index);

  override def setSingle(index: Int) {
    currentDomain = Interval(index, index)
    removed = true
  }

  override def value(index: Int) = index + offset

  override def remove(index: Int) {
    assert(present(index));
    if (index == currentDomain.lb) currentDomain = Interval(currentDomain.lb + 1, currentDomain.ub)
    else if (index == currentDomain.ub) currentDomain = Interval(currentDomain.lb, currentDomain.ub - 1)
    else throw new IllegalArgumentException
    removed = true
  }

  override def removeFrom(lb: Int) = {

    val nbRemVals = math.max(0, currentDomain.ub - lb + 1)
    currentDomain = Interval(currentDomain.lb, currentDomain.ub - nbRemVals)
    removed = nbRemVals > 0
    nbRemVals;

  }

  override def removeTo(ub: Int) = {
    val nbRemVals = math.max(0, ub - currentDomain.lb + 1)
    currentDomain = Interval(currentDomain.lb + nbRemVals, ub)
    removed = nbRemVals > 0
    nbRemVals;
  }

  override def getBitVector = throw new UnsupportedOperationException

  override def setLevel(level: Int) {
    assert(level > currentLevel, "Given level " + level
      + " should be greater than current " + currentLevel)
    if (removed) {
      history ::= (currentLevel, currentDomain)
      removed = false
    }
    currentLevel = level;
  }

  override def restoreLevel(level: Int) {
    assert(level <= currentLevel);

    history = history.dropWhile(_._1 > level)
    if (history == Nil) {
      currentDomain = initDomain
    } else {
      currentDomain = history.head._2
    }
    currentLevel = level;
    //    _last = bvDomain.prevSetBit(domain.length);

  }

  override def getAtLevel(level: Int) = throw new UnsupportedOperationException
  //  {
  //    if (level < currentLevel) {
  //      history.find(_._1 <= level) match {
  //        case Some(e) => e._2
  //        case _ => BitVector.newBitVector(domain.length, true);
  //      }
  //    } else bvDomain;
  //  }

  override def allValues = initDomain.allValues.toArray

  override def toString = "[" + currentDomain.lb + ", " + currentDomain.ub + "]"

}
