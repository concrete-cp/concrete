package cspfj.problem;

import cspfj.util.BitVector
import java.util.Arrays
import cspfj.util.Backtrackable
import scala.annotation.tailrec

final object BitVectorDomain {
  val DISPLAYED_VALUES = 4;

  def fullBV(size: Int) = {
    val bv = BitVector.newBitVector(size)
    bv.fill(true)
    bv
  }

  def bv(lb: Int, ub: Int, hole: Int) = {
    val bv = fullBV(ub + 1)
    bv.clearTo(lb)
    bv.clear(hole)
    bv
  }
}

final class BitVectorDomain(val bvDomain: BitVector, val size: Int) extends IntSet {

  def this(size: Int) = {
    this(BitVectorDomain.fullBV(size), size)
  }

  def this(lb: Int, ub: Int, hole: Int) = {
    this(BitVectorDomain.bv(lb, ub, hole), ub - lb)
  }

  //  private def size_=(s: Int) {
  //    _size = s
  //  }

  val first = bvDomain.nextSetBit(0)

  val last = bvDomain.lastSetBit
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

  override def next(i: Int) = bvDomain.nextSetBit(i + 1)

  override def prev(i: Int) = bvDomain.prevSetBit(i)

  def closestLeq(i: Int): Int = bvDomain.prevSetBit(i + 1)

  def closestGeq(i: Int): Int = bvDomain.nextSetBit(i)

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = bvDomain.get(index);

  def setSingle(index: Int) = new IntervalDomain(index, index)

  def filter(f: Int => Boolean) = {
    val nbv = bvDomain.clone

    @tailrec
    def filt(i: Int, s: Int): Int =
      if (i < 0) s
      else if (f(i)) filt(next(i), s)
      else {
        nbv.clear(i)
        filt(next(i), s - 1)
      }

    val s = filt(first, size)
    if (s != size) new BitVectorDomain(nbv, s)
    else this
  }

  def remove(index: Int) = {
    assert(present(index));
    val nbv = bvDomain.clone
    nbv.clear(index);
    //    if (_first == index) nbv._first = bvDomain.nextSetBit(index)
    //    if (_last == index) nbv._last = bvDomain.prevSetBit(index)
    //    size -= 1;
    new BitVectorDomain(nbv, size - 1)
  }

  def removeFrom(lb: Int) = {
    val nbv = bvDomain.clone
    val nbRemVals = nbv.clearFrom(lb)
    if (nbRemVals > 0)
      new BitVectorDomain(nbv, size - nbRemVals)
    else this
  }

  def removeTo(ub: Int) = {
    val nbv = bvDomain.clone
    val nbRemVals = nbv.clearTo(ub + 1)
    if (nbRemVals > 0)
      new BitVectorDomain(nbv, size - nbRemVals)
    else this
  }

  def toString(id: Indexer) = if (size <= BitVectorDomain.DISPLAYED_VALUES) {
    iterator.map(id.value).mkString("{", ", ", "}");
  } else {
    iterator.map(id.value).take(BitVectorDomain.DISPLAYED_VALUES).mkString("{", ", ", " (" + (size - BitVectorDomain.DISPLAYED_VALUES) + " more)}")
  }

  def subsetOf(d: IntSet) = d match {
    case d: BitVectorDomain => bvDomain.subsetOf(d.bvDomain)
    case d: IntervalDomain => first >= d.first && last <= d.last
  }

  def toBitVector = bvDomain
  def intersects(bv: BitVector) = bv.intersects(bvDomain)
  def intersects(bv: BitVector, part: Int) = bv.intersects(bvDomain, part)
  def bound = false
}
