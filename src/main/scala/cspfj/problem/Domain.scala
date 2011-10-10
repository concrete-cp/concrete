package cspfj.problem;

import cspfj.util.BitVector;

trait Domain extends Iterable[Int] {
  def next(i: Int): Int

  def prev(i: Int): Int

  def lastAbsent: Int

  def prevAbsent(i: Int): Int

  def index(value: Int): Int

  def value(index: Int): Int

  def firstIndex: Int

  def lastIndex: Int

  override def head = firstIndex
  //override def last = lastIndex
  override def isEmpty = firstIndex >= 0

  def maxSize: Int

  def present(index: Int): Boolean

  def setSingle(index: Int)

  def remove(index: Int)
  
  def size: Int

  /**
   * @param lb
   * @return Removes all indexes starting from given lower bound.
   */
  def removeFrom(lb: Int): Int

  /**
   * @param ub
   * @return Removes all indexes up to given upper bound.
   */
  def removeTo(ub: Int): Int

  def setLevel(level: Int)

  def restoreLevel(level: Int)
  
  def reset() { restoreLevel(0) }

  def getAtLevel(level: Int): BitVector
  
  def firstValue = value(firstIndex)

  def allValues: Array[Int]

  def currentValues: Array[Int]

  /**
   * @param value
   * @return the index of the closest value lower or equal to the given value.
   */
  def closestLeq(value: Int): Int

  /**
   * @param value
   * @return the index of the closest value greater or equal to the given
   *         value.
   */
  def closestGeq(value: Int): Int

  def getBitVector: BitVector
  
  def currentIndexes: Array[Int]

  def iterator = new Iterator[Int] {
    var index = firstIndex

    override def hasNext = index >= 0

    override def next = {
      val current = index
      index = Domain.this.next(index);
      current
    }
  }

}
