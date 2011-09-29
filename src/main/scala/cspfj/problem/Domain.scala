package cspfj.problem;

import cspfj.util.BitVector;

trait Domain extends Iterable[Int] {
  def first(): Int

  def last(): Int

  def next(i: Int): Int

  def prev(i: Int): Int

  def lastAbsent(): Int

  def prevAbsent(i: Int): Int

  def size(): Int

  def index(value: Int): Int

  def value(index: Int): Int

  def maxSize(): Int

  def present(index: Int): Boolean

  def setSingle(index: Int)

  def remove(index: Int)

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

  def setLevel(level: Int): Int

  def restoreLevel(level: Int)

  def getAtLevel(level: Int): BitVector

  def allValues(): Array[Int]

  def currentValues(): Array[Int]

  /**
   * @param value
   * @return the index of the closest value lower or equal to the given value.
   */
  def greatest(value: Int): Int

  /**
   * @param value
   * @return the index of the closest value greater or equal to the given
   *         value.
   */
  def lowest(value: Int): Int

  def getBitVector(): BitVector

  def copy(): Domain

  def iterator = new Iterator[Int] {
    var index = first

    override def hasNext = index >= 0

    override def next = {
      val current = index
      index = Domain.this.next(index);
      current
    }

    override def remove() = throw new UnsupportedOperationException();
  }

}
