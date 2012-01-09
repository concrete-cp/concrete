package cspfj.problem;

import cspfj.util.BitVector
import java.util.Arrays
import cspfj.util.Backtrackable

final object BitVectorDomain {
  val HISTORY_INCREMENT = 20;
  val DISPLAYED_VALUES = 4;
}

final class BitVectorDomain(
  private val domain: Array[Int]) extends Domain with Backtrackable[(BitVector, Int)] {
  require(domain.sliding(2).forall(p => p.size == 1 || p(0) < p(1)), "Only ordered domains are supported");

  private val indicesMap = domain.zipWithIndex.map { case (v, i) => v -> i }.toMap.withDefaultValue(-1)

  private var _size = domain.size

  def size = _size

  private val bvDomain = BitVector.newBitVector(domain.length, true)

  //  def this(domain: BitVectorDomain) =
  //    this(domain.domain, domain.bvDomain.clone, domain.history)
  //
  //  def this(domain: Array[Int]) = this(domain, BitVector.newBitVector(domain.length, true))

  def this(domain: Int*) = this(domain.toArray)

  def save() = (bvDomain.clone, size)

  def restore(data: (BitVector, Int)) {
    data._1.copyTo(bvDomain)
    _size = data._2
  }

  def getAtLevel(level: Int) = getLevel(level)._1

  override def first = bvDomain.nextSetBit(0);

  override def last = bvDomain.prevSetBit(maxSize)

  override def lastAbsent = bvDomain.prevClearBit(domain.length);

  override def next(i: Int) = bvDomain.nextSetBit(i + 1)

  override def prev(i: Int) = bvDomain.prevSetBit(i)

  override def prevAbsent(i: Int) = bvDomain.prevClearBit(i)

  /**
   * @param value
   *            the value we seek the index for
   * @return the index of the given value or -1 if it could not be found
   */
  def index(value: Int) = indicesMap(value)

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
    _size = 1;
  }

  def value(index: Int) = domain(index);

  def remove(index: Int) {
    assert(present(index));
    altering()
    _size -= 1;
    bvDomain.clear(index);
  }

  def removeFrom(lb: Int) = {
    altering()
    val nbRemVals = bvDomain.clearFrom(lb);
    _size -= nbRemVals;
    nbRemVals;
  }

  def removeTo(ub: Int) = {
    altering()
    val nbRemVals = bvDomain.clearTo(ub + 1);
    _size -= nbRemVals;
    nbRemVals;
  }

  def getBitVector = bvDomain

  val allValues = domain;

  override val maxSize = domain.size

  override def toString = if (size <= BitVectorDomain.DISPLAYED_VALUES) {
    values.mkString("[", ", ", "]");
  } else {
    values.take(BitVectorDomain.DISPLAYED_VALUES).mkString("[", ", ", " (" + (size - BitVectorDomain.DISPLAYED_VALUES) + " more)]")
  }

  override def subset(d: Domain) = d match {
    //case d: BitVectorDomain => d.bvDomain.isSubSet(bvDomain)
    case d => super.subset(d)
  }

}
