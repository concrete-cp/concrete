package cspfj.problem;

import cspfj.util.BitVector
import java.util.Arrays
import cspfj.util.Backtrackable

final object BitVectorDomain {
  val DISPLAYED_VALUES = 4;
}

final class BitVectorDomain(val bvDomain: BitVector, val initSize: Int) extends IntSet {

  def this(size: Int) = {
    this(BitVector.newBitVector(size), size)
    bvDomain.fill(true)
  }

  def this(lb: Int, ub: Int, hole: Int) = {
    this(ub + 1)
    if (lb > 0)
      removeTo(lb - 1)
    remove(hole)
  }

  private var _size = initSize

  def size = _size

  private def size_=(s: Int) {
    _size = s
    if (s == 0) throw Domain.empty
  }

  var _first = 0

  var _last = _size - 1

  def first = {
    assert(_first == bvDomain.nextSetBit(0))
    _first
  }

  def last = {
    assert(_last == bvDomain.prevSetBit(initSize))
    _last
  }

  def copy = new BitVectorDomain(bvDomain.clone, size)

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

  def remove(index: Int) = {
    assert(present(index));
    bvDomain.clear(index);
    if (_first == index) _first = bvDomain.nextSetBit(index)
    if (_last == index) _last = bvDomain.prevSetBit(index)
    size -= 1;
    this
  }

  def removeFrom(lb: Int) = {
    val nbRemVals = bvDomain.clearFrom(lb);
    _last = bvDomain.prevSetBit(lb)
    size -= nbRemVals;
    this
  }

  def removeTo(ub: Int) = {
    val nbRemVals = bvDomain.clearTo(ub + 1);
    _first = bvDomain.nextSetBit(ub)
    size -= nbRemVals;
    this
  }

  def getBitVector = bvDomain

  def toString(id: Indexer) = if (size <= BitVectorDomain.DISPLAYED_VALUES) {
    iterator.map(id.value).mkString("[", ", ", "]");
  } else {
    iterator.map(id.value).take(BitVectorDomain.DISPLAYED_VALUES).mkString("[", ", ", " (" + (size - BitVectorDomain.DISPLAYED_VALUES) + " more)]")
  }

  def subsetOf(d: IntSet) = d match {
    case d: BitVectorDomain => bvDomain.subsetOf(d.bvDomain)
    case d: IntervalDomain => first >= d.first && last <= d.last
  }

  def toBitVector = bvDomain

}
