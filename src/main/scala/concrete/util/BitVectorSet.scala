package concrete.util

import scala.annotation.tailrec

final object BitVectorSet {
  val DISPLAYED_VALUES = 5;

}

final class BitVectorSet(val bv: BitVector, val size: Int) extends IntSet {
  require(size >= 2, "BitVectorSets must have at least two elements")
  assert(bv.cardinality == size, bv + " : " + bv.cardinality + " != " + size)

  def this(size: Int) = {
    this(BitVector.filled(size), size)
  }

  def this(lb: Int, ub: Int, hole: Int) = {
    this(BitVector.intBvH(lb, ub, hole), ub - lb)
  }

  val first = bv.nextSetBit(0)

  assert(first >= 0)

  val last = bv.lastSetBit

  assert(last >= 0)

  override def next(i: Int) = bv.nextSetBit(i + 1)

  override def prev(i: Int) = bv.prevSetBit(i)

  def closestLeq(i: Int): Int = bv.prevSetBit(i + 1)

  def closestGeq(i: Int): Int = bv.nextSetBit(i)

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = bv(index);

  def filter(f: Int => Boolean) = {
    val newBv = bv.filter(f)
    if (newBv == bv) {
      this
    } else {
      IntSet.ofBV(newBv, newBv.cardinality)
    }
  }

  def remove(index: Int) = {
    assert(present(index));
    IntSet.ofBV(bv - index, size - 1)
  }

  def removeFrom(lb: Int) = {
    val newBv = bv.clearFrom(lb)
    if (newBv == bv) {
      this
    } else {
      IntSet.ofBV(newBv, newBv.cardinality)
    }
  }

  def removeTo(ub: Int) = {
    val nbv = bv.clearUntil(ub + 1)
    if (nbv == bv) {
      this
    } else {
      IntSet.ofBV(nbv, nbv.cardinality)
    }
  }

  def toString(id: Indexer) =
    if (size <= BitVectorSet.DISPLAYED_VALUES) {
      iterator.map(id.value).mkString("{", ", ", "}");
    } else {
      iterator.map(id.value).take(BitVectorSet.DISPLAYED_VALUES - 1)
        .mkString("{", ", ", ", [" + (size - BitVectorSet.DISPLAYED_VALUES) + "...], " + id.value(last) + "}")
    }

  def subsetOf(d: IntSet) = d match {
    case d: BitVectorSet => bv.subsetOf(d.bv)
    case d: IntervalSet => first >= d.first && last <= d.last
  }

  def toBitVector = bv
  def intersects(that: BitVector) = bv.intersects(that)
  def intersects(that: BitVector, part: Int) = bv.intersects(that, part)
  def bound = false
  def isEmpty = false
}
