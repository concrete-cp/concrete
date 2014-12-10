package concrete

import concrete.util.BitVector
import concrete.util.Interval
import com.typesafe.scalalogging.LazyLogging

final class IntervalDomain(val span: Interval) extends IntDomain with LazyLogging {

  def this(lb: Int, ub: Int) = this(Interval(lb, ub))

  val length = span.size

  require(size >= 2, "Intervals must have at least two elements, use Singleton instead")

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

  def bitVector(offset: Int) = {
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

  def bound = true
  override def isEmpty = false

}
