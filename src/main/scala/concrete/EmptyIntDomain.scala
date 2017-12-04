package concrete

import bitvectors.BitVector


object EmptyIntDomain extends IntDomain {
  def size = 0

  override def head = throw new NoSuchElementException

  override def last = throw new NoSuchElementException

  def next(i: Int) = throw new NoSuchElementException

  def prev(i: Int) = throw new NoSuchElementException

  def prevOrEq(i: Int) = throw new NoSuchElementException

  def nextOrEq(i: Int) = throw new NoSuchElementException

  def present(i: Int): Boolean = {
    Domain.checks += 1
    false
  }

  def remove(i: Int): IntDomain = this

  def removeFrom(lb: Int): IntDomain = this

  override def removeAfter(lb: Int): IntDomain = this

  def removeTo(ub: Int): IntDomain = this

  override def removeUntil(ub: Int): IntDomain = this

  override def filter(f: Int => Boolean): IntDomain = this

  def filterBounds(f: Int => Boolean): IntDomain = this

  override def toString = "[]"

  def convex = throw new IllegalStateException

  override def isEmpty = true

  def apply(i: Int) = throw new NoSuchElementException

  def toBitVector(offset: Int): BitVector = BitVector.empty

  def span = throw new NoSuchElementException

  def singleValue = throw new NoSuchElementException

  def &(d: Domain): Domain = EmptyIntDomain

  def |(d: Domain): Domain = d

  def |(v: Int): IntDomain = Singleton(v)

  def &(lb: Int, ub: Int): Domain = EmptyIntDomain

  def median = throw new NoSuchElementException

  def isAssigned = throw new UnsupportedOperationException

  def shift(o: Int): IntDomain = EmptyIntDomain

  def disjoint(d: Domain) = true

  def foreach[S](f: Int => S): Unit = ()

  def subsetOf(d: Domain) = true
}