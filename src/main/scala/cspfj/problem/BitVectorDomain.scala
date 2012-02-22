package cspfj.problem;

import cspfj.util.BitVector
import java.util.Arrays
import cspfj.util.Backtrackable

final object BitVectorDomain {
  val HISTORY_INCREMENT = 20;
  val DISPLAYED_VALUES = 4;
}

final class BitVectorDomain(
  domain: Array[Int]) extends Domain with Backtrackable[(BitVector, Int)] {
  require(domain.sliding(2).forall(p => p.size == 1 || p(0) < p(1)), "Only ordered domains are supported");

  if (domain.size == 0) throw Domain.empty

  override val maxSize = domain.size

  private val indexer = Indexer.factory(domain)

  private var _size = domain.size

  def size = _size

  private def size_=(s: Int) {
    _size = s
    if (s == 0) throw Domain.empty
  }

  private val bvDomain = BitVector.newBitVector(domain.length)
  bvDomain.fill(true)

  //  def this(domain: BitVectorDomain) =
  //    this(domain.domain, domain.bvDomain.clone, domain.history)
  //
  //  def this(domain: Array[Int]) = this(domain, BitVector.newBitVector(domain.length, true))

  def this(domain: Int*) = this(domain.toArray)

  def save() = (bvDomain.clone, size)

  def restore(data: (BitVector, Int)) {
    data._1.copyTo(bvDomain)
    _size = data._2
    _first = bvDomain.nextSetBit(0);
    _last = bvDomain.prevSetBit(maxSize)
  }

  def getAtLevel(level: Int) = getLevel(level)._1

  var _first = bvDomain.nextSetBit(0);

  var _last = bvDomain.prevSetBit(maxSize)

  def first = {
    assert(_first == bvDomain.nextSetBit(0))
    _first
  }

  def last = {
    assert(_last == bvDomain.prevSetBit(maxSize))
    _last
  }

  override def lastAbsent = bvDomain.prevClearBit(domain.length);

  override def next(i: Int) = bvDomain.nextSetBit(i + 1)

  override def prev(i: Int) = bvDomain.prevSetBit(i)

  override def prevAbsent(i: Int) = bvDomain.prevClearBit(i)

  /**
   * @param value
   *            the value we seek the index for
   * @return the index of the given value or -1 if it could not be found
   */
  def index(value: Int) = indexer.index(value)

  private def closestLeq(value: Int, lb: Int, ub: Int): Int = {
    if (domain(ub) <= value) {
      ub
    } else {
      val test = (ub + lb) / 2
      if (domain(test) > value) {
        closestLeq(value, lb, test - 1)
      } else {
        closestLeq(value, test + 1, ub)
      }
    }
  }

  def closestLeq(value: Int): Int =
    if (domain(first) > value) -1 else closestLeq(value, first, last)

  private def closestGeq(value: Int, lb: Int, ub: Int): Int = {
    if (domain(lb) >= value) {
      lb
    } else {
      val test = (ub + lb) / 2;
      if (domain(test) >= value) {
        closestGeq(value, lb, test - 1)
      } else {
        closestGeq(value, test + 1, ub)
      }
    }
  }

  def closestGeq(value: Int): Int =
    if (domain(last) < value) -1 else closestGeq(value, first, last)

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = bvDomain.get(index);

  def setSingle(index: Int) {
    altering()
    bvDomain.setSingle(index);
    _first = index
    _last = index
    _size = 1;
  }

  def value(index: Int) = indexer.value(index);

  def valueBV(offset: Int) =
    indexer match {
      case i: DirectIndices if offset == 0 => bvDomain.clone
      case i: OffsetIndices if offset == i.offset => bvDomain.clone
      case _ => {
        val bv = BitVector.newBitVector(lastValue - offset + 1)
        values.foreach(vl => bv.set(vl - offset))
        bv
      }
    }

  def remove(index: Int) {
    assert(present(index));
    altering()
    bvDomain.clear(index);
    if (_first == index) _first = bvDomain.nextSetBit(index)
    if (_last == index) _last = bvDomain.prevSetBit(index)

    size -= 1;
  }

  def removeFrom(lb: Int) = {
    altering()
    val nbRemVals = bvDomain.clearFrom(lb);
    _last = bvDomain.prevSetBit(lb)
    size -= nbRemVals;
    nbRemVals;
  }

  def removeTo(ub: Int) = {
    altering()
    val nbRemVals = bvDomain.clearTo(ub + 1);
    _first = bvDomain.nextSetBit(ub)
    size -= nbRemVals;
    nbRemVals;
  }

  def getBitVector = bvDomain

  val allValues = domain;

  override def toString = if (size <= BitVectorDomain.DISPLAYED_VALUES) {
    values.mkString("[", ", ", "]");
  } else {
    values.take(BitVectorDomain.DISPLAYED_VALUES).mkString("[", ", ", " (" + (size - BitVectorDomain.DISPLAYED_VALUES) + " more)]")
  }

  override def subsetOf(d: Domain) = d match {
    case d: BitVectorDomain => bvDomain.subsetOf(d.bvDomain)
    case _ => super.subsetOf(d)
  }

}
