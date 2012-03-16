package cspfj.problem;

import cspfj.util.BitVector
import java.util.Arrays
import cspfj.util.Backtrackable
import scala.annotation.tailrec

final object BitVectorSet {
  val DISPLAYED_VALUES = 5;

  def fullBV(size: Int) = {
    val bv = BitVector.newBitVector(size)
    bv.fill(true)
    bv
  }

  def intBv(lb: Int, ub: Int): BitVector = {
    val bv = fullBV(ub + 1)
    bv.clearTo(lb)
    bv
  }

  def intBvH(lb: Int, ub: Int, hole: Int): BitVector = {
    val bv = intBv(lb, ub)
    bv.clear(hole)
    bv
  }
}

final class BitVectorSet(val bv: BitVector, val size: Int) extends IntSet {

  def this(size: Int) = {
    this(BitVectorSet.fullBV(size), size)
  }

  def this(lb: Int, ub: Int, hole: Int) = {
    this(BitVectorSet.intBvH(lb, ub, hole), ub - lb)
  }

  //  private def size_=(s: Int) {
  //    _size = s
  //  }

  val first = bv.nextSetBit(0)

  val last = bv.lastSetBit
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

  def filter(f: Int => Boolean) = {
    val nbv = bv.clone

    @tailrec
    def filt(i: Int, s: Int): Int =
      if (i < 0) s
      else if (f(i)) filt(next(i), s)
      else {
        nbv.clear(i)
        filt(next(i), s - 1)
      }

    val s = filt(first, size)
    if (s == size) this
    else IntSet.ofBV(nbv, s)
  }

  def remove(index: Int) = {
    assert(present(index));
    val nbv = bv.clone
    nbv.clear(index);
    //    if (_first == index) nbv._first = bvDomain.nextSetBit(index)
    //    if (_last == index) nbv._last = bvDomain.prevSetBit(index)
    //    size -= 1;
    new BitVectorSet(nbv, size - 1)
  }

  def removeFrom(lb: Int) = {
    val nbv = bv.clone
    val ch = nbv.clearFrom(lb)
    
    if (ch)
      new BitVectorSet(nbv, nbv.cardinality)
    else this
  }

  def removeTo(ub: Int) = {
    val nbv = bv.clone
    val ch = nbv.clearTo(ub + 1)
    if (ch)
      new BitVectorSet(nbv, nbv.cardinality)
    else this
  }

  def toString(id: Indexer) = if (size <= BitVectorSet.DISPLAYED_VALUES) {
    iterator.map(id.value).mkString("{", ", ", "}");
  } else {
    iterator.map(id.value).take(BitVectorSet.DISPLAYED_VALUES).mkString("{", ", ", " (" + (size - BitVectorSet.DISPLAYED_VALUES) + " more)}")
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
