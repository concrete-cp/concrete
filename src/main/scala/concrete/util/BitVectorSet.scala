package concrete.util

import scala.annotation.tailrec

final object BitVectorSet {
  val DISPLAYED_VALUES = 5;


}

final class BitVectorSet(val bv: BitVector, val size: Int) extends IntSet {
  require(size >= 2, "BitVectorSets must have at least two elements")
  assert(bv.cardinality == size, bv + " : " + bv.cardinality + " != " + size)

  def this(size: Int) = {
    this(BitVector.fullBV(size), size)
  }

  def this(lb: Int, ub: Int, hole: Int) = {
    this(BitVector.intBvH(lb, ub, hole), ub - lb)
  }

  //  private def size_=(s: Int) {
  //    _size = s
  //  }

  val first = bv.nextSetBit(0)

  assert(first >= 0)

  val last = bv.lastSetBit

  assert(last >= 0)

  //
  //  def first = {
  //    assert(_first == bvDomain.nextSetBit(0))
  //    _first
  //  }
  //
  //  def last = {
  //    assert(_last == bvDomain.lastSetBit)
  //    _last
  //  }

  def copy = this //new BitVectorDomain(bvDomain.clone, size)

  override def next(i: Int) = bv.nextSetBit(i + 1)

  override def prev(i: Int) = bv.prevSetBit(i)

  def closestLeq(i: Int): Int = bv.prevSetBit(i + 1)

  def closestGeq(i: Int): Int = bv.nextSetBit(i)

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = bv.get(index);

  private def filter(f: Int => Boolean, nbv: BitVector): Int = {
    var i = first
    var s = size
    while (i >= 0) {
      if (!f(i)) {
        nbv.clear(i)
        s -= 1
      }
      i = next(i)
    }
    s
  }

  def filter(f: Int => Boolean) = {
    val nbv = bv.clone
    val s = filter(f, nbv)
    if (s == size) this else IntSet.ofBV(nbv, s)
  }

  def remove(index: Int) = {
    assert(present(index));
    val nbv = bv.clone
    nbv.clear(index);
    IntSet.ofBV(nbv, size - 1)
  }

  def removeFrom(lb: Int) = {
    val nbv = bv.clone
    val ch = nbv.clearFrom(lb)

    if (ch) { IntSet.ofBV(nbv, nbv.cardinality) }
    else { this }
  }

  def removeTo(ub: Int) = {
    val nbv = bv.clone
    val ch = nbv.clearTo(ub + 1)
    if (ch) { IntSet.ofBV(nbv, nbv.cardinality) }
    else { this }
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
