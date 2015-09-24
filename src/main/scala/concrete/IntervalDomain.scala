package concrete

import cspom.util.BitVector
import concrete.util.Interval
import com.typesafe.scalalogging.LazyLogging

final class IntervalDomain(val span: Interval) extends IntDomain with LazyLogging {

  def this(lb: Int, ub: Int) = this(Interval(lb, ub))

  val length = span.size

  require(size >= 2, "Intervals must have at least two elements, use Singleton instead")

  def singleValue = throw new IllegalStateException

  override def head = span.lb

  override def last = span.ub

  def next(i: Int) = if (i >= span.ub) throw new NoSuchElementException else i + 1

  def prev(i: Int) = if (i <= span.lb) throw new NoSuchElementException else i - 1

  def prevOrEq(i: Int): Int =
    if (i < head) { -1 }
    else if (i > last) { last }
    else { i }

  def nextOrEq(i: Int): Int =
    if (i > last) { -1 }
    else if (i < head) { head }
    else { i }

  def copy = this
  
  def isAssigned = false

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = span.contains(index);

  def remove(index: Int) = {
    if (index == span.lb) { IntDomain.ofInterval(span.lb + 1, span.ub) }
    else if (index == span.ub) { IntDomain.ofInterval(span.lb, span.ub - 1) }
    else if (present(index)) { toBVDomain.remove(index) }
    else this
  }

  def removeFrom(lb: Int) =
    if (lb > span.ub) { this }
    else { IntDomain.ofInterval(span.lb, lb - 1) }

  def removeAfter(lb: Int) =
    if (lb >= span.ub) { this }
    else { IntDomain.ofInterval(span.lb, lb) }

  def removeTo(ub: Int) =
    if (ub < span.lb) { this }
    else { IntDomain.ofInterval(ub + 1, span.ub) }

  def removeUntil(ub: Int) =
    if (ub <= span.lb) { this }
    else { IntDomain.ofInterval(ub, span.ub) }

  override def filter(f: Int => Boolean) = {
    val filt = toBVDomain.filter(f)
    if (filt eq toBVDomain) {
      this
    } else {
      filt
    }
  }

  def filterBounds(f: Int => Boolean) = {
    var lb = head
    var ub = last
    while (lb <= ub && !f(lb)) {
      lb += 1
    }
    while (ub > lb && !f(ub)) {
      ub -= 1
    }
    if (lb == head && ub == last) {
      this
    } else {
      IntDomain.ofInterval(lb, ub)
    }
  }

  def apply(i: Int) = {
    if (i < size) i + head else throw new IndexOutOfBoundsException
  }

  override def toString() = s"[$head, $last]"

  def subsetOf(d: IntDomain) = d match {
    case d: BitVectorDomain => (head to last).forall(d.present)
    case d: IntervalDomain  => head >= d.head && last <= d.last
  }

  lazy val toBVDomain = {
    new BitVectorDomain(head, bitVector0, size)
  }

  lazy val bitVector0 = BitVector.empty.set(0, size)

  var requestedOffset: Int = _
  var requestedBV: BitVector = null

  def toBitVector(offset: Int) = {
    if (offset == head) {
      bitVector0
    } else if (requestedBV != null && offset == requestedOffset) {
      requestedBV
    } else {

      requestedOffset = offset
      requestedBV = BitVector.empty.set(head - offset, last - offset + 1)
      logger.info(s"generating BV for $this offset $offset: $requestedBV")
      requestedBV
    }
  }

  def &(d: Domain): Domain = d match {
    case id: IntervalDomain => this & id.span
    case s: Singleton       => if (present(s.singleValue)) s else EmptyIntDomain
    case bd: BitVectorDomain =>
      val domain = bd & span
      if (domain.size == size) {
        this
      } else {
        domain
      }
    case EmptyIntDomain   => EmptyIntDomain
    case b: BooleanDomain => b & this
  }

  def &(lb: Int, ub: Int) = {
    val nlb = math.max(span.lb, lb)
    val nub = math.min(span.ub, ub)
    if (nlb == span.lb && nub == span.ub) {
      this
    } else {
      IntDomain.ofInterval(nlb, nub)
    }
  }

  def |(d: Domain): Domain = d match {
    case id: IntervalDomain  => this | id.span

    case s: Singleton        => this | s.singleValue

    case bv: BitVectorDomain => bv | span

    case EmptyIntDomain      => this

    case b: BooleanDomain    => this | b.span
  }

  def |(i1: Interval) = {
    val i2 = span

    if (i1 connected i2) {
      new IntervalDomain(i1 span i2)
    } else {
      val offset = math.min(i1.lb, i2.lb)
      val union = toBitVector(offset).set(i1.lb - offset, i1.ub - offset + 1)
      IntDomain.ofBitVector(offset, union, union.cardinality)
    }
  }

  def |(value: Int): IntDomain = {
    if (present(value)) {
      this
    } else {
      if (value == head - 1) {
        IntDomain.ofInterval(value, last)
      } else if (value == last + 1) {
        IntDomain.ofInterval(head, value)
      } else {
        val offset = math.min(value, head)
        val union = toBitVector(offset) + (value - offset)
        IntDomain.ofBitVector(offset, union, size + 1)
      }
    }
  }

  def convex = true
  override def isEmpty = false

  def iterator = span.allValues.iterator

  def median = (head + last + 1) / 2

}
