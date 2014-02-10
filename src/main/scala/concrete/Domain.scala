package concrete

import concrete.util.BitVector
import concrete.util.Interval
import concrete.util.IntSet
import cspom.Statistic;

object Domain {
  @Statistic
  var checks = 0L
}

abstract class Domain {

  def next(i: Int): Int

  def prev(i: Int): Int

  def index(value: Int): Int

  def value(index: Int): Int

  def first: Int

  def last: Int

  def isEmpty: Boolean = first < 0

  def maxSize: Int //= allValues.size

  def present(index: Int): Boolean

  def presentVal(value: Int): Boolean = {
    val i = index(value)
    i >= 0 && present(i)
  }

  def setSingle(index: Int): Unit

  def remove(index: Int): Unit

  def removeVal(v: Int): Boolean = {
    val i = index(v)
    if (i >= 0 && present(i)) {
      remove(i)
      true
    } else {
      false
    }
  }

  def size: Int

  /**
   * @param lb
   * @return Removes all indexes starting from given lower bound.
   */
  def removeFrom(lb: Int): Boolean

  def removeFromVal(lb: Int): Boolean = {
    val v = closestGeq(lb)
    v >= 0 && removeFrom(v)
  }

  /**
   * @param ub
   * @return Removes all indexes up to given upper bound.
   */
  def removeTo(ub: Int): Boolean

  def removeToVal(ub: Int): Boolean = {
    val v = closestLeq(ub)
    v >= 0 && removeTo(v)
  }

  def filter(f: Int => Boolean): Boolean

  def filterValues(f: Int => Boolean): Boolean = filter(i => f(value(i)))

  def setLevel(level: Int): Unit

  def restoreLevel(level: Int): Unit

  def currentLevel: Int

  def getAtLevel(level: Int): BitVector

  def firstValue: Int = value(first)

  def lastValue: Int = value(last)

  //def allValues: Array[Int]

  override def equals(o: Any) = o match {
    case d: Domain => valuesIterator.sameElements(d.valuesIterator)
    case _ => false
  }

  override def hashCode = values.hashCode()

  /**
   * @param value
   * @return the index of the closest value strictly lower than the given value.
   */
  def closestLt(value: Int): Int

  /**
   * @param value
   * @return the index of the closest value strictly greater than the given
   *         value.
   */
  def closestGt(value: Int): Int

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

  val indices = new Traversable[Int] {
    def foreach[B](f: Int => B) {
      var i = first
      while (i >= 0) {
        f(i)
        i = next(i)
      }
    }
  }

  def indices(from: Int): Traversable[Int] = new Traversable[Int] {
    def foreach[B](f: Int => B) {
      var i = if (present(from)) from else next(from)
      while (i >= 0) {
        f(i)
        i = next(i)
      }
    }
  }

  def indicesIterator: Iterator[Int] = indicesIterator(first)

  def indicesIterator(from: Int): Iterator[Int] = new Iterator[Int] {
    var index = from

    override def hasNext = index >= 0

    override def next = {
      val current = index
      index = Domain.this.next(index);
      current
    }
  }

  def indicesR: Iterator[Int] = new Iterator[Int] {
    var index = Domain.this.last

    override def hasNext = index >= 0

    override def next = {
      val current = index
      index = Domain.this.prev(index);
      current
    }
  }

  def valuesIterator: Iterator[Int] = indicesIterator map value

  val values = new Traversable[Int] {
    def foreach[B](f: Int => B) {
      var i = first
      while (i >= 0) {
        f(value(i))
        i = next(i)
      }
    }
  }

  def valueInterval: Interval = Interval(firstValue, lastValue)

  def intersect(a: Int, b: Int): Boolean = removeTo(a - 1) | removeFrom(b + 1)

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

  def removeValInterval(lb: Int, ub: Int) {

    var i = closestGeq(lb)
    val end = closestLeq(ub)

    if (i >= 0 && end >= 0) {
      while (i <= end) {
        if (present(i)) { remove(i) }
        i += 1
      }
    }
  }

  def disjoint(d: Domain, i: Int = first): Boolean = i < 0 || {
    val i2 = d.index(value(i))
    (i2 < 0 || !d.present(i2)) && disjoint(d, next(i))
  }

  def intersects(bv: BitVector): Int
  def intersects(bv: BitVector, part: Int): Boolean
  def bound: Boolean
  def intSet: IntSet
  def undefined: Boolean

}
