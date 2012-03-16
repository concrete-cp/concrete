package cspfj.problem

import cspfj.util.BitVector

object EmptyDomain extends IntSet {
  def size = 0
  def copy = this
  def first = throw new NoSuchElementException
  def last = throw new NoSuchElementException
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
  def toBitVector = BitVector.newBitVector(0)
  def intersects(bv: BitVector) = -1
  def intersects(bv: BitVector, part: Int) = false
  def bound = throw new IllegalStateException
  def isEmpty = true
}