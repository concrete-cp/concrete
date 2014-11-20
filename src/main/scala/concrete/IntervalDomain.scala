package concrete

import concrete.util.BitVector
import concrete.util.Interval

final class IntervalDomain(val domain: Interval) extends IntDomain {

  def this(lb: Int, ub: Int) = this(Interval(lb, ub))

  val length = domain.size

  require(size >= 2, "Intervals must have at least two elements, use Singleton instead")

  override def head = domain.lb

  override def last = domain.ub

  def next(i: Int) = if (i >= domain.ub) throw new NoSuchElementException else i + 1

  def prev(i: Int) = if (i <= domain.lb) throw new NoSuchElementException else i - 1

  def prevOrEq(i: Int): Int =
    if (i < head) { -1 }
    else if (i > last) { last }
    else { i }

  def nextOrEq(i: Int): Int =
    if (i > last) { -1 }
    else if (i < head) { head }
    else { i }

  def copy = this

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = domain.contains(index);

  def remove(index: Int) = {
    if (index == domain.lb) { IntDomain.ofInterval(domain.lb + 1, domain.ub) }
    else if (index == domain.ub) { IntDomain.ofInterval(domain.lb, domain.ub - 1) }
    else if (present(index)) { new BitVectorDomain(domain.lb, domain.ub, index) }
    else this
  }

  def removeFrom(lb: Int) =
    if (lb > domain.ub) { this }
    else { IntDomain.ofInterval(domain.lb, lb - 1) }

  def removeTo(ub: Int) =
    if (ub < domain.lb) { this }
    else { IntDomain.ofInterval(ub + 1, domain.ub) }

  override def filter(f: Int => Boolean) = {
    val bv = toBitVector
    val nbv = bv.filter(f)
    if (nbv == bv) {
      this
    } else {
      IntDomain.ofBV(0, nbv, nbv.cardinality)
    }
  }

  override def toString() = s"[$head, $last]"

  def subsetOf(d: IntDomain) = d match {
    case d: BitVectorDomain => (head to last).forall(d.present)
    case d: IntervalDomain  => head >= d.head && last <= d.last
  }

  lazy val toBitVector = {
    require(head >= 0)
    val bv = BitVector.intBv(head, last)
    assert(bv.cardinality == size, this + " -> " + bv)
    bv
  }

  def intersects(bv: BitVector) = bv.intersects(toBitVector)
  def intersects(bv: BitVector, part: Int) = bv.intersects(toBitVector, part)
  def bound = true
  override def isEmpty = false

}
