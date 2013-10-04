package concrete

import concrete.util.BitVector
import concrete.util.IntSet

final object UndefinedDomain extends Domain {
  def bound: Boolean = throw new UnsupportedOperationException()
  def closestGeq(value: Int): Int = throw new UnsupportedOperationException()
  def closestLeq(value: Int): Int = throw new UnsupportedOperationException()
  def closestGt(value: Int): Int = ???
  def closestLt(value: Int): Int = ???
  def currentLevel: Int = throw new UnsupportedOperationException()
  def filter(f: Int => Boolean): Boolean = throw new UnsupportedOperationException()
  def first: Int = throw new UnsupportedOperationException()
  def getAtLevel(level: Int): BitVector = throw new UnsupportedOperationException()
  def index(value: Int): Int = throw new UnsupportedOperationException()
  def intSet: IntSet = throw new UnsupportedOperationException()
  def intersects(bv: BitVector, part: Int): Boolean = throw new UnsupportedOperationException()
  def intersects(bv: BitVector): Int = throw new UnsupportedOperationException()
  def last: Int = throw new UnsupportedOperationException()
  def maxSize: Int = throw new UnsupportedOperationException()
  def next(i: Int): Int = throw new UnsupportedOperationException()
  def present(index: Int): Boolean = throw new UnsupportedOperationException()
  def prev(i: Int): Int = throw new UnsupportedOperationException()
  def remove(index: Int): Unit = throw new UnsupportedOperationException()
  def removeFrom(lb: Int): Boolean = throw new UnsupportedOperationException()
  def removeTo(ub: Int): Boolean = throw new UnsupportedOperationException()
  def restoreLevel(level: Int): Unit = throw new UnsupportedOperationException()
  def setLevel(level: Int): Unit = throw new UnsupportedOperationException()
  def setSingle(index: Int): Unit = throw new UnsupportedOperationException()
  def size: Int = throw new UnsupportedOperationException()
  def value(index: Int): Int = throw new UnsupportedOperationException()
  override def toString = "[?]"
  def undefined = true
  override def equals(o: Any) = o match {
    case o: AnyRef => o eq this
    case _ => false
  }
}
