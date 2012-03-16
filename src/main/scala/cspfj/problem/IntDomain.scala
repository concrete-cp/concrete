package cspfj.problem;

import cspfj.util.BitVector
import java.util.Arrays
import cspfj.util.Backtrackable

object IntDomain {
  def apply(d: Int*): IntDomain = new IntDomain(IntSet.factory(d), Indexer.factory(d))

  def apply(r: Range): IntDomain =
    if (r.step == 1) new IntDomain(
      new IntervalSet(0, r.last - r.start),
      Indexer.ofInt(r.start, r.last))
    else apply(r: _*)
}

final class IntDomain(
  var intSet: IntSet,
  val indexer: Indexer) extends Domain with Backtrackable[IntSet] {

  override val maxSize = intSet.size

  def size = intSet.size

  //  def this(domain: BitVectorDomain) =
  //    this(domain.domain, domain.bvDomain.clone, domain.history)
  //
  //  def this(domain: Array[Int]) = this(domain, BitVector.newBitVector(domain.length, true))

  def save() = intSet.copy

  def restore(data: IntSet) {
    intSet = data
  }

  def getAtLevel(level: Int) = getLevel(level).toBitVector

  def first = intSet.first

  def last = intSet.last

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
  def present(index: Int) = intSet.present(index);

  def setSingle(index: Int) {
    altering()
    intSet = new Singleton(index)
  }

  def value(index: Int) = indexer.value(index);

  def remove(index: Int) {
    assert(present(index));
    altering()
    intSet = intSet.remove(index)
    if (size == 0) throw Domain.empty
  }

  def removeFrom(lb: Int) = {
    val s = intSet.size
    intSet = intSet.removeFrom(lb)
    val r = s - intSet.size
    if (r > 0) {
      altering()
      if (r == s) throw Domain.empty
      r
    } else 0
  }

  def removeTo(ub: Int) = {
    val s = intSet.size
    intSet = intSet.removeTo(ub)
    val r = s - intSet.size
    if (r > 0) {
      altering()
      if (r == s) throw Domain.empty
      r
    } else 0
  }

  def filter(f: Int => Boolean) = {
    val is = intSet.filter(f)
    if (is eq intSet) false
    else {
      intSet = is
      altering()
      if (size == 0) throw Domain.empty
      true
    }
  }

  def bound = intSet.bound

  def intersects(bv: BitVector) = intSet.intersects(bv)
  def intersects(bv: BitVector, part: Int) = intSet.intersects(bv, part)
  //def allValues = domain

  override def toString = intSet.toString(indexer)

}
