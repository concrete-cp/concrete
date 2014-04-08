package concrete

import concrete.util.Backtrackable
import concrete.util.BitVector
import concrete.util.Indexer
import concrete.util.IntSet
import concrete.util.Interval
import concrete.util.IntervalSet
import concrete.util.Singleton
import scala.collection.SortedSet

object IntDomain {
  @annotation.varargs
  def apply(d: Int*): IntDomain = new IntDomain(IntSet(d.size), Indexer.factory(d))

  def apply(d: SortedSet[Int]): IntDomain = apply(d.toSeq: _*)

  def apply(r: Range): IntDomain =
    if (r.step == 1 && r.size > 1) {
      apply(Interval(r.start, r.last))
    } else {
      apply(r: _*)
    }

  def apply(i: Interval): IntDomain = {
    new IntDomain(
      new IntervalSet(0, i.ub - i.lb),
      Indexer.ofInterval(i.lb, i.ub))
  }

}

final class IntDomain(
  private var _intSet: IntSet,
  val indexer: Indexer) extends Domain with Backtrackable[IntSet] {

  override val maxSize = intSet.size

  var size = intSet.size

  def save = intSet.copy

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
    if (value(ub) <= v) { ub }
    else {
      val test = (ub + lb) / 2
      if (value(test) > v) {
        closestLeq(v, lb, test - 1)
      } else {
        closestLeq(v, test + 1, ub)
      }
    }
  }

  def closestLeq(value: Int): Int = {
    val i = index(value)
    if (i < 0) {
      if (value < firstValue) {
        -1
      } else {
        closestLeq(value, first, last)
      }
    } else {
      intSet.closestLeq(i)
    }
  }

  private def closestLt(v: Int, lb: Int, ub: Int): Int = {
    if (value(ub) < v) { ub }
    else {
      val test = (ub + lb) / 2
      if (value(test) >= v) {
        closestLt(v, lb, test - 1)
      } else {
        closestLt(v, test + 1, ub)
      }
    }
  }

  def closestLt(value: Int): Int = {
    val i = index(value)
    if (i < 0) {
      if (value <= firstValue) {
        -1
      } else {
        closestLt(value, first, last)
      }
    } else {
      intSet.closestLt(i)
    }
  }

  private def closestGeq(v: Int, lb: Int, ub: Int): Int = {
    if (value(lb) >= v) {
      lb
    } else {
      val test = (ub + lb) / 2;
      if (value(test) >= v) {
        closestGeq(v, lb, test - 1)
      } else {
        closestGeq(v, test + 1, ub)
      }
    }
  }

  def closestGeq(value: Int): Int = {
    val i = index(value)
    if (i < 0) {
      if (value > lastValue) {
        -1
      } else {
        closestGeq(value, first, last)
      }
    } else {
      intSet.closestGeq(i)
    }
  }

  private def closestGt(v: Int, lb: Int, ub: Int): Int = {
    if (value(lb) > v) {
      lb
    } else {
      val test = (ub + lb) / 2;
      if (value(test) > v) {
        closestGt(v, lb, test - 1)
      } else {
        closestGt(v, test + 1, ub)
      }
    }
  }

  def closestGt(value: Int): Int = {
    val i = index(value)
    if (i < 0) {
      if (value >= lastValue) {
        -1
      } else {
        closestGt(value, first, last)
      }
    } else {
      intSet.closestGt(i)
    }
  }

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = {
    Domain.checks += 1
    _intSet.present(index)
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
    if (intSet.isEmpty) throw EmptyDomain
  }

  def removeFrom(lb: Int) = {
    val s = intSet.removeFrom(lb)
    if (s eq intSet) {
      false
    } else {
      altering()
      intSet = s
      if (s.isEmpty) throw EmptyDomain
      true
    }
  }

  def removeTo(ub: Int) = {
    val s = intSet.removeTo(ub)
    if (s eq intSet) {
      false
    } else {
      altering()
      intSet = s
      if (s.isEmpty) throw EmptyDomain
      true
    }
  }

  def filter(f: Int => Boolean) = {
    val is = intSet.filter(f)
    if (is eq intSet) {
      false
    } else {
      intSet = is
      altering()
      if (intSet.isEmpty) throw EmptyDomain
      true
    }
  }

  def bound = intSet.bound

  def intersects(bv: BitVector) = intSet.intersects(bv)
  def intersects(bv: BitVector, part: Int) = intSet.intersects(bv, part)
  //def allValues = domain

  override def toString = intSet.toString(indexer)
  def undefined = false
}
