package concrete

import concrete.util.BitVector
import concrete.util.Interval
import concrete.util.Math
import com.typesafe.scalalogging.LazyLogging
object BitVectorDomain {
  val DISPLAYED_VALUES = 5;

}

final class BitVectorDomain(val offset: Int, val bitVector: BitVector, override val length: Int)
  extends IntDomain with LazyLogging {
  require(size >= 2, "BitVectorSets must have at least two elements")
  Math.checkedAdd(offset, bitVector.lastSetBit)

  assert(bitVector.cardinality == size, bitVector + " : " + bitVector.cardinality + " != " + size)

  //  def this(size: Int) = {
  //    this(0, BitVector.filled(size), size)
  //  }

  override val head = offset + bitVector.nextSetBit(0)

  assert(head >= offset)

  override val last = offset + bitVector.lastSetBit

  assert(last >= offset)

  override def next(i: Int) = {
    val b = math.max(0, i - offset + 1)
    val n = bitVector.nextSetBit(b)
    if (n < 0) throw new NoSuchElementException else offset + n
  }

  override def prev(i: Int) = {
    val b = math.max(0, i - offset)
    val n = bitVector.prevSetBit(b)
    if (n < 0) throw new NoSuchElementException else offset + n
  }

  def prevOrEq(i: Int): Int = offset + bitVector.prevSetBit(i - offset + 1)

  def nextOrEq(i: Int): Int = offset + bitVector.nextSetBit(i - offset)

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(value: Int) = {
    val bit = value - offset
    bit >= 0 && bitVector(value - offset)
  }

  override def filter(f: Int => Boolean) = {
    val newbitVector = bitVector.filter(i => f(i + offset))
    if (newbitVector == bitVector) {
      this
    } else {
      IntDomain.ofBitVector(offset, newbitVector, newbitVector.cardinality)
    }
  }

  def remove(index: Int) = {
    if (present(index)) {
      IntDomain.ofBitVector(offset, bitVector - (index - offset), size - 1)
    } else { this }
  }

  def removeFrom(lb: Int) = {
    val b = math.max(0, lb - offset)
    val newbitVector = bitVector.clearFrom(b)
    if (newbitVector == bitVector) {
      this
    } else {
      IntDomain.ofBitVector(offset, newbitVector, newbitVector.cardinality)
    }
  }

  def removeAfter(lb: Int) = removeFrom(lb + 1)

  def removeUntil(ub: Int) = {
    val b = math.max(0, ub - offset)
    val nbitVector = bitVector.clearUntil(b)
    if (nbitVector == bitVector) {
      this
    } else {
      IntDomain.ofBitVector(offset, nbitVector, nbitVector.cardinality)
    }
  }

  def removeTo(ub: Int) = removeUntil(ub + 1)

  override def toString =
    if (size <= BitVectorDomain.DISPLAYED_VALUES) {
      iterator.mkString("{", ", ", "}");
    } else {
      iterator.take(BitVectorDomain.DISPLAYED_VALUES - 1)
        .mkString("{", ", ", ", [" + (size - BitVectorDomain.DISPLAYED_VALUES) + "...], " + last + "}")
    }

  def subsetOf(d: IntDomain) = d match {
    case d: BitVectorDomain => bitVector.subsetOf(d.bitVector)
    case d: IntervalDomain  => head >= d.head && last <= d.last
  }

  def apply(i: Int) = iterator.drop(i - 1).next

  var requestedOffset: Int = _
  var requestedBV: BitVector = null

  def bitVector(offset: Int) = {
    if (offset == this.offset) {
      bitVector
    } else if (requestedBV != null && offset == requestedOffset) {
      requestedBV
    } else {
      logger.info(s"generating BV from offset ${this.offset} to $offset")
      requestedOffset = offset
      requestedBV = BitVector.empty
      for (v <- this) {
        requestedBV += v - offset
      }
      requestedBV
    }
  }

  lazy val span = Interval(head, last)

  //  def intersects(that: BitVector) = bitVector.intersects(that)
  //  def intersects(that: BitVector, part: Int) = bitVector.intersects(that, part)
  def bound = false
  override def isEmpty = false
}
