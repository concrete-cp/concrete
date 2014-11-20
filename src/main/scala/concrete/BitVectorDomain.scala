package concrete

import concrete.util.BitVector

object BitVectorDomain {
  val DISPLAYED_VALUES = 5;

}

final class BitVectorDomain(val offset: Int, val bv: BitVector, override val length: Int) extends IntDomain {
  require(size >= 2, "BitVectorSets must have at least two elements")
  assert(bv.cardinality == size, bv + " : " + bv.cardinality + " != " + size)

  def this(size: Int) = {
    this(0, BitVector.filled(size), size)
  }

  def this(lb: Int, ub: Int, hole: Int) = {
    this(0, BitVector.intBvH(lb, ub, hole), ub - lb)
  }

  override val head = offset + bv.nextSetBit(0)

  assert(head >= offset)

  override val last = offset + bv.lastSetBit

  assert(last >= offset)

  override def next(i: Int) = {
    val b = math.max(0, i - offset + 1)
    val n = bv.nextSetBit(b)
    if (n < 0) throw new NoSuchElementException else offset + n
  }

  override def prev(i: Int) = {
    val b = math.max(0, i - offset)
    val n = bv.prevSetBit(b)
    if (n < 0) throw new NoSuchElementException else offset + n
  }

  def prevOrEq(i: Int): Int = offset + bv.prevSetBit(i - offset + 1)

  def nextOrEq(i: Int): Int = offset + bv.nextSetBit(i - offset)

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(value: Int) = bv(value - offset);

  override def filter(f: Int => Boolean) = {
    val newBv = bv.filter(i => f(i + offset))
    if (newBv == bv) {
      this
    } else {
      IntDomain.ofBV(offset, newBv, newBv.cardinality)
    }
  }

  def remove(index: Int) = {
    if (present(index)) {
      IntDomain.ofBV(offset, bv - index, size - 1)
    } else { this }
  }

  def removeFrom(lb: Int) = {
    val b = math.max(0, lb - offset)
    val newBv = bv.clearFrom(b)
    if (newBv == bv) {
      this
    } else {
      IntDomain.ofBV(offset, newBv, newBv.cardinality)
    }
  }

  def removeTo(ub: Int) = {
    val b = math.max(0, ub - offset)
    val nbv = bv.clearUntil(b + 1)
    if (nbv == bv) {
      this
    } else {
      IntDomain.ofBV(offset, nbv, nbv.cardinality)
    }
  }

  override def toString =
    if (size <= BitVectorDomain.DISPLAYED_VALUES) {
      iterator.mkString("{", ", ", "}");
    } else {
      iterator.take(BitVectorDomain.DISPLAYED_VALUES - 1)
        .mkString("{", ", ", ", [" + (size - BitVectorDomain.DISPLAYED_VALUES) + "...], " + last + "}")
    }

  def subsetOf(d: IntDomain) = d match {
    case d: BitVectorDomain => bv.subsetOf(d.bv)
    case d: IntervalDomain  => head >= d.head && last <= d.last
  }

  def toBitVector = bv
  def intersects(that: BitVector) = bv.intersects(that)
  def intersects(that: BitVector, part: Int) = bv.intersects(that, part)
  def bound = false
  override def isEmpty = false
}
