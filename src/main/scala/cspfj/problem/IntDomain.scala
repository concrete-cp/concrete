package cspfj.problem;

import cspfj.util.BitVector
import java.util.Arrays
import cspfj.util.Backtrackable

final class IntDomain(
  domain: Array[Int]) extends Domain with Backtrackable[IntSet] {

  if (domain.size == 0) throw Domain.empty

  private var intSet = IntSet.factory(domain)

  override val maxSize = domain.size

  private val indexer = Indexer.factory(domain)

  def size = intSet.size

  //  def this(domain: BitVectorDomain) =
  //    this(domain.domain, domain.bvDomain.clone, domain.history)
  //
  //  def this(domain: Array[Int]) = this(domain, BitVector.newBitVector(domain.length, true))

  def this(domain: Int*) = this(domain.toArray)

  def save() = intSet.copy

  def restore(data: IntSet) {
    intSet = data
  }

  def getAtLevel(level: Int) = getLevel(level).toBitVector

  def first = intSet.first

  def last = intSet.last

  def next(i: Int) = intSet.next(i)

  def prev(i: Int) = intSet.prev(i)

  /**
   * @param value
   *            the value we seek the index for
   * @return the index of the given value or -1 if it could not be found
   */
  def index(value: Int) = indexer.index(value)

  def closestLeq(value: Int): Int = {
    val i = index(value)
    if (i < 0) -1
    else
      intSet.closestLeq(i)
  }

  def closestGeq(value: Int): Int = {
    val i = index(value)
    if (i < 0) -1
    else
      intSet.closestGeq(i)
  }

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(index: Int) = intSet.present(index);

  def setSingle(index: Int) {
    altering()
    intSet = intSet.setSingle(index)
  }

  def value(index: Int) = indexer.value(index);

  def remove(index: Int) {
    assert(present(index));
    altering()
    intSet = intSet.remove(index)
  }

  def removeFrom(lb: Int) = {
    altering()
    val s = intSet.size
    intSet = intSet.removeFrom(lb)
    intSet.size - s
  }

  def removeTo(ub: Int) = {
    altering()
    val s = intSet.size
    intSet = intSet.removeTo(ub)
    intSet.size - s
  }

  def getBitVector = intSet.getBitVector

  val allValues = domain;

  override def toString = intSet.toString

}
