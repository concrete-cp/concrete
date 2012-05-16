package cspfj

import cspfj.util.Backtrackable
import cspfj.util.BitVector
import cspfj.util.Indexer
import cspfj.util.IntSet
import cspfj.util.Interval
import cspfj.util.IntervalSet
import cspfj.util.Singleton

object IntDomain {
  @annotation.varargs
  def apply(d: Int*): IntDomain = new IntDomain(IntSet.factory(d), Indexer.factory(d))

  //def apply(d: Array[Int]): IntDomain = apply(d: _*)

  def apply(r: Range): IntDomain =
    if (r.step == 1) new IntDomain(
      new IntervalSet(0, r.last - r.start),
      Indexer.ofInterval(r.start, r.last))
    else apply(r: _*)

  //var s: Map[Class[_ <: IntSet], Int] = Map.empty.withDefaultValue(0)

}

final class IntDomain(
  private var _intSet: IntSet,
  val indexer: Indexer) extends Domain with Backtrackable[IntSet] {

  override val maxSize = intSet.size

  var size = intSet.size

  def save() = intSet.copy

  def restore(data: IntSet) {
    intSet = data
  }

  def getAtLevel(level: Int) = getLevel(level).toBitVector

  var first = intSet.first

  var last = intSet.last

  override def isEmpty = intSet.isEmpty

  def intSet_=(is: IntSet) {
    first = is.first
    last = is.last
    size = is.size
    _intSet = is
    if (!isEmpty) {
      _firstValue = value(first)
      _lastValue = value(last)
      _valueInterval = Interval(firstValue, lastValue)
    }
  }

  var _firstValue = value(first)
  var _lastValue = value(last)
  var _valueInterval = Interval(firstValue, lastValue)

  override def firstValue = {
    assert(!isEmpty)
    _firstValue
  }

  override def lastValue = {
    assert(!isEmpty)
    _lastValue
  }

  override def valueInterval = {
    assert(!isEmpty)
    _valueInterval
  }

  def intSet = _intSet

  def next(i: Int) = intSet.next(i)

  def prev(i: Int) = intSet.prev(i)

  /**
   * @param value
   *            the value we seek the index for
   * @return the index of the given value or -1 if it could not be found
   */
  def index(value: Int) = indexer.index(value)

  private def closestLeq(v: Int, lb: Int, ub: Int): Int = {
    if (value(ub) <= v) ub
    else {
      val test = (ub + lb) / 2
      if (value(test) > v) {
        closestLeq(v, lb, test - 1)
      } else {
        closestLeq(v, test + 1, ub)
      }
    }
  }

  private def closestGeq(v: Int, lb: Int, ub: Int): Int = {
    if (value(lb) >= v) lb
    else {
      val test = (ub + lb) / 2;
      if (value(test) >= v)
        closestGeq(v, lb, test - 1)
      else
        closestGeq(v, test + 1, ub)
    }
  }

  def closestLeq(value: Int): Int = {
    val i = index(value)
    if (i < 0) {
      if (value < firstValue) -1
      else closestLeq(value, first, last)
    } else intSet.closestLeq(i)
  }

  def closestGeq(value: Int): Int = {
    val i = index(value)
    if (i < 0) {
      if (value > lastValue) -1
      else closestGeq(value, first, last)
    } else intSet.closestGeq(i)
  }

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = {
    //    val c = IntDomain.s(intSet.getClass) + 1
    //    IntDomain.s += intSet.getClass -> c
    //    if (c % 10000 == 0) println(IntDomain.s)
    intSet.present(index);
  }

  def setSingle(index: Int) {
    assert(present(index))
    assert(size > 1)
    altering()
    intSet = new Singleton(index)
  }

  def value(index: Int) = indexer.value(index);

  def remove(index: Int) {
    assert(present(index));
    altering()
    intSet = intSet.remove(index)
    if (intSet.isEmpty) throw Domain.empty
  }

  def removeFrom(lb: Int) = {
    val s = intSet.removeFrom(lb)
    if (s eq intSet) false
    else {
      altering()
      intSet = s
      if (s.isEmpty) throw Domain.empty
      true
    }
  }

  def removeTo(ub: Int) = {
    val s = intSet.removeTo(ub)
    if (s eq intSet) false
    else {
      altering()
      intSet = s
      if (s.isEmpty) throw Domain.empty
      true
    }
  }

  def filter(f: Int => Boolean) = {
    val is = intSet.filter(f)
    if (is eq intSet) false
    else {
      intSet = is
      altering()
      if (intSet.isEmpty) throw Domain.empty
      true
    }
  }

  def bound = intSet.bound

  def intersects(bv: BitVector) = intSet.intersects(bv)
  def intersects(bv: BitVector, part: Int) = intSet.intersects(bv, part)
  //def allValues = domain

  override def toString = intSet.toString(indexer)

}
