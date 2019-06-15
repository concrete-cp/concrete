package concrete

import bitvectors.BitVector
import concrete.util.Interval


object EmptyIntDomain extends IntDomain {
  override def size = 0

  override def head = throw new NoSuchElementException

  override def last = throw new NoSuchElementException

  def next(i: Int) = throw new NoSuchElementException

  def prev(i: Int) = throw new NoSuchElementException

  def prevOrEq(i: Int) = throw new NoSuchElementException

  def nextOrEq(i: Int) = throw new NoSuchElementException

  def contains(i: Int): Boolean = {
    Domain.checks += 1
    false
  }

  def excl(i: Int): IntDomain = this

  def removeFrom(lb: Int): IntDomain = this

  override def removeAfter(lb: Int): IntDomain = this

  def removeTo(ub: Int): IntDomain = this

  override def removeUntil(ub: Int): IntDomain = this

  override def filter(f: Int => Boolean): IntDomain = this

  def filterBounds(f: Int => Boolean): IntDomain = this

  override def toString = "[]"

  def convex = throw new IllegalStateException

  override def isEmpty = true

  def toBitVector(offset: Int): BitVector = BitVector.empty

  def span = throw new NoSuchElementException

  override def spanSlice(from: Option[Int], to: Option[Int]): Option[Interval] = None

  def singleValue = throw new NoSuchElementException

  def &(d: Domain): Domain = EmptyIntDomain

  def |(d: Domain): Domain = d

  def |(v: Int): IntDomain = Singleton(v)

  def &(lb: Int, ub: Int): Domain = EmptyIntDomain

  def median = throw new NoSuchElementException

  def isAssigned = throw new UnsupportedOperationException

  def shift(o: Int): IntDomain = EmptyIntDomain

  def disjoint(d: Domain) = true

  override def foreach[S](f: Int => S): Unit = ()

  def subsetOf(d: Domain) = true

  def iterator: Iterator[Int] = Iterator.empty

  def iteratorFrom(start: Int): Iterator[Int] = Iterator.empty
}