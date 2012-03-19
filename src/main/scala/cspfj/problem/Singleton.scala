package cspfj.problem;

import cspfj.util.BitVector

final class Singleton(val index: Int) extends IntSet {
  
  require(index >= 0)

  val size = 1

  def first = index

  def last = index
  
  

  def next(i: Int) = if (i < index) index else -1

  def prev(i: Int) = if (i > index) index else -1

  def closestLeq(i: Int): Int =
    if (i >= index) index
    else -1

  def closestGeq(i: Int): Int =
    if (i <= index) index
    else -1

  def copy = this

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(i: Int) = i == index

  def setSingle(index: Int) = throw new IllegalStateException

  def remove(i: Int) = {
    assert(index == i)
    EmptyIntSet
  }

  def removeFrom(lb: Int) =
    if (lb > index) this
    else EmptyIntSet

  def removeTo(ub: Int) =
    if (ub < index) this
    else EmptyIntSet

  def filter(f: Int => Boolean) =
    if (f(index)) this
    else EmptyIntSet

  def toString(id: Indexer) = "[" + id.value(index) + "]"

  def subsetOf(d: IntSet) = d.present(index)

  lazy val toBitVector = {
    val bv = BitVector.newBitVector(index + 1)
    bv.set(index)
    bv
  }

  def intersects(bv: BitVector) = {
    val part = index >> 6
    if (bv.get(index)) part else -1
  }

  def intersects(bv: BitVector, part: Int) = bv.get(index)
  def bound = true
  def isEmpty = false
}
