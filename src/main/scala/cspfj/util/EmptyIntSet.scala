package cspfj.util

object EmptyIntSet extends IntSet {
  def size = 0
  def copy = this
  def first = -1
  def last = -1
  def next(i: Int) = -1
  def prev(i: Int) = -1
  def closestLeq(i: Int) = -1
  def closestGeq(i: Int) = -1
  def present(i: Int) = false
  def remove(i: Int) = throw new IllegalStateException
  def removeFrom(lb: Int) = this
  def removeTo(ub: Int) = this
  def filter(f: Int => Boolean) = this
  def subsetOf(d: IntSet) = true
  def toString(id: Indexer) = "[]"
  val toBitVector = BitVector.newBitVector(0)
  def intersects(bv: BitVector) = -1
  def intersects(bv: BitVector, part: Int) = false
  def bound = throw new IllegalStateException
  def isEmpty = true
}