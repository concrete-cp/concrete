package concrete

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.util.{CacheOne, Interval}

final class IntervalDomain(val span: Interval) extends IntDomain with LazyLogging {

  private lazy val toBVDomain = {
    new BitVectorDomain(head, bitVector0, size)
  }
  private lazy val bitVector0 = BitVector.filled(size)

  assert(size >= 2, "Intervals must have at least two elements, use Singleton instead")
  private val offsetBV = new CacheOne[Int, BitVector]()

  def this(lb: Int, ub: Int) = this(Interval(lb, ub))

  def singleValue = throw new IllegalStateException

  def next(i: Int): Int = if (i >= span.ub) throw new NoSuchElementException else i + 1

  def prev(i: Int): Int = if (i <= span.lb) throw new NoSuchElementException else i - 1

  override def spanSlice(from: Option[Int], to: Option[Int]): Option[Interval] = {
    val lb = from.map(span.from).getOrElse(spanOption)
    lb.flatMap(s => to.map(s.to).getOrElse(lb))
  }

  def spanOption = Some(span)

  def isAssigned = false

  def excl(index: Int): IntDomain = {

    if (index == span.lb) {
      IntDomain.ofInterval(span.lb + 1, span.ub)
    } else if (index == span.ub) {
      IntDomain.ofInterval(span.lb, span.ub - 1)
    } else if (contains(index)) {
      toBVDomain excl index
    } else {
      this
    }

  }

  def removeFrom(lb: Int): IntDomain =
    if (lb > span.ub) {
      this
    }
    else {
      IntDomain.ofInterval(span.lb, lb - 1)
    }

  def removeAfter(lb: Int): IntDomain =
    if (lb >= span.ub) {
      this
    }
    else {
      IntDomain.ofInterval(span.lb, lb)
    }

  def removeTo(ub: Int): IntDomain =
    if (ub < span.lb) {
      this
    }
    else {
      IntDomain.ofInterval(ub + 1, span.ub)
    }

  def removeUntil(ub: Int): IntDomain =
    if (ub <= span.lb) {
      this
    }
    else {
      IntDomain.ofInterval(ub, span.ub)
    }

  override def filter(f: Int => Boolean): Domain = {
    val filt = toBVDomain.filter(f)
    if (filt eq toBVDomain) {
      this
    } else {
      filt
    }
  }

  def filterBounds(f: Int => Boolean): IntDomain = {
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

  override def toString: String = s"[$head, $last]"

  def subsetOf(d: IntDomain): Boolean = d match {
    case d: BitVectorDomain => (head to last).forall(d)
    case d: IntervalDomain => head >= d.head && last <= d.last
  }

  def toBitVector(offset: Int): BitVector = {
    if (offset == head) {
      bitVector0
    } else {
      offsetBV(offset, {
        logger.info(s"generating BV for $this offset $offset from ${Thread.currentThread.getStackTrace.mkString(", ")}")
        BitVector.empty.set(head - offset, last - offset + 1)
      })
    }
  }

  def shift(o: Int): IntDomain = if (o == 0) this else
    new IntervalDomain(span + o)

  def &(d: Domain): Domain = d match {
    case id: IntervalDomain => this & id.span
    case s: Singleton => if (contains(s.head)) s else EmptyIntDomain
    case EmptyIntDomain => EmptyIntDomain
    case b: BooleanDomain => b & this
    case _ =>
      val domain = d & span
      if (domain.size == size) {
        this
      } else {
        domain
      }
  }

  /**
    * @param index
    * index to test
    * @return true iff index is present
    */
  def contains(index: Int): Boolean = {
    Domain.checks += 1
    span.contains(index)
  }

  override def size: Int = span.size

  def disjoint(d: Domain): Boolean = last < d.head || head > d.last

  def &(lb: Int, ub: Int): Domain = {
    val nlb = math.max(span.lb, lb)
    val nub = math.min(span.ub, ub)
    if (nlb == span.lb && nub == span.ub) {
      this
    } else {
      IntDomain.ofInterval(nlb, nub)
    }
  }

  def |(d: Domain): Domain = d match {
    case id: IntervalDomain => this | id.span

    case s: Singleton => this | s.singleValue

    case EmptyIntDomain | BooleanDomain.EMPTY => this

    case b: BooleanDomain => this | b.span

    case _ => d | this
  }

  def |(i1: Interval): IntDomain = {
    val i2 = span
    if (i1 subsetOf i2) {
      this
    } else if (i1 connected i2) {
      new IntervalDomain(i1 span i2)
    } else {
      val offset = math.min(i1.lb, i2.lb)
      val union = toBitVector(offset).set(i1.lb - offset, i1.ub - offset + 1)
      IntDomain.ofBitVector(offset, union, union.cardinality)
    }
  }

  def |(value: Int): IntDomain = {
    if (contains(value)) {
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

  def subsetOf(d: Domain): Boolean = {
    d match {
      case EmptyIntDomain | BooleanDomain.EMPTY | _: Singleton => assert(size > 1); false
      case d@(_: IntervalDomain | _: BooleanDomain) => head >= d.head && last <= d.last
      case _ => (d & span).size == size
    }
  }

  override def head: Int = span.lb

  override def last: Int = span.ub

  def convex = true

  override def isEmpty = false

  def iterator: Iterator[Int] = span.allValues.iterator

  def iteratorFrom(start: Int): Iterator[Int] = (start to last).iterator

  override def foreach[U](f: Int => U): Unit = {
    span.allValues.foreach(f)
  }

  def median: Int = (head + last + 1) / 2

}
