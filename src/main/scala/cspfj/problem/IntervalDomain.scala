package cspfj.problem;

import cspfj.util.BitVector
import java.util.Arrays
import cspfj.util.Backtrackable
import cspfj.util.Interval

final class IntervalDomain(val domain: Interval) extends IntSet {

  def this(lb: Int, ub: Int) = this(Interval(lb, ub))

  val size = domain.size

  def first = domain.lb

  def last = domain.ub

  def next(i: Int) = if (i >= domain.ub) -1 else i + 1

  def prev(i: Int) = if (i <= domain.lb) -1 else i - 1

  def closestLeq(i: Int): Int =
    if (i < first) -1
    else if (i > last) last
    else i

  def closestGeq(i: Int): Int =
    if (i > last) -1
    else if (i < first) first
    else i

  def copy = this

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = domain.in(index);

  def setSingle(index: Int) = new SingleDomain(index)

  def remove(index: Int) = {
    assert(present(index));
    if (index == domain.lb) IntSet.ofInt(domain.lb + 1, domain.ub)
    else if (index == domain.ub) IntSet.ofInt(domain.lb, domain.ub - 1)
    else new BitVectorDomain(domain.lb, domain.ub, index)
  }

  def removeFrom(lb: Int) =
    if (lb > domain.ub) this
    else IntSet.ofInt(domain.lb, lb - 1)

  def removeTo(ub: Int) =
    if (ub < domain.lb) this
    else IntSet.ofInt(ub + 1, domain.ub)

  def filter(f: Int => Boolean) = {
    val bv = new BitVectorDomain(toBitVector, size)
    val nbv = bv.filter(f)
    if (bv eq nbv) this else nbv
  }

  def toString(id: Indexer) =
    if (domain.lb > domain.ub) "[]"
    else if (domain.lb == domain.ub) "[" + id.value(domain.lb) + "]"
    else "[" + id.value(domain.lb) + ", " + id.value(domain.ub) + "]"

  def subsetOf(d: IntSet) = d match {
    case d: BitVectorDomain => (first to last).forall(d.present)
    case d: IntervalDomain => first >= d.first && last <= d.last
  }

  lazy val toBitVector = BitVectorDomain.intBv(first, last)

  def intersects(bv: BitVector) = bv.intersects(toBitVector)
  def intersects(bv: BitVector, part: Int) = bv.intersects(toBitVector, part)
  def bound = true
}
