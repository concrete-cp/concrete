package concrete.util

import cspom.variable.Interval

final class IntervalSet(val domain: Interval) extends IntSet {

  def this(lb: Int, ub: Int) = this(Interval(lb, ub))

  require(domain.lb >= 0)

  val size = domain.size

  require(size >= 2, "Invervals must have at least two elements, use Singleton instead")

  val first = domain.lb

  val last = domain.ub

  def next(i: Int) = if (i >= domain.ub) -1 else i + 1

  def prev(i: Int) = if (i <= domain.lb) -1 else i - 1

  def closestLeq(i: Int): Int =
    if (i < first) { -1 }
    else if (i > last) { last }
    else { i }

  def closestGeq(i: Int): Int =
    if (i > last) { -1 }
    else if (i < first) { first }
    else { i }

  def copy = this

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = domain.contains(index);

  def remove(index: Int) = {
    assert(present(index));
    if (index == domain.lb) { IntSet.ofInterval(domain.lb + 1, domain.ub) }
    else if (index == domain.ub) { IntSet.ofInterval(domain.lb, domain.ub - 1) }
    else { new BitVectorSet(domain.lb, domain.ub, index) }
  }

  def removeFrom(lb: Int) =
    if (lb > domain.ub) { this }
    else { IntSet.ofInterval(domain.lb, lb - 1) }

  def removeTo(ub: Int) =
    if (ub < domain.lb) { this }
    else { IntSet.ofInterval(ub + 1, domain.ub) }

  def filter(f: Int => Boolean) = {
    val nbv = bitVectorSet.filter(f)
    if (bitVectorSet eq nbv) this else nbv
  }

  def toString(id: Indexer) =
    if (domain.lb > domain.ub) { "[]" }
    else if (domain.lb == domain.ub) { s"[${id.value(domain.lb)}]" }
    else { s"[${id.value(domain.lb)}, ${id.value(domain.ub)}]" }

  def subsetOf(d: IntSet) = d match {
    case d: BitVectorSet => (first to last).forall(d.present)
    case d: IntervalSet => first >= d.first && last <= d.last
  }

  val toBitVector = {

    val bv = BitVector.intBv(first, last)
    assert(bv.cardinality == size, this + " -> " + bv)
    bv
  }

  val bitVectorSet = new BitVectorSet(toBitVector, size)

  def intersects(bv: BitVector) = bv.intersects(toBitVector)
  def intersects(bv: BitVector, part: Int) = bv.intersects(toBitVector, part)
  def bound = true
  def isEmpty = false

  override def toString = "[" + first + ", " + last + "]"
}
