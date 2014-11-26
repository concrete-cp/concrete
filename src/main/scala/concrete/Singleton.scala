package concrete

import concrete.util.BitVector
import scala.collection.mutable.HashMap

object Singleton {
  val cache = new HashMap[Int, Singleton]
  def apply(v: Int) = cache.getOrElseUpdate(v, new Singleton(v))
}

final class Singleton(val value: Int) extends IntDomain {

  def length = 1

  override def head = value

  override def last = value

  def next(i: Int) = if (i < value) value else throw new NoSuchElementException

  def prev(i: Int) = if (i > value) value else throw new NoSuchElementException

  def prevOrEq(i: Int): Int =
    if (i >= value) { value }
    else { throw new NoSuchElementException }

  def nextOrEq(i: Int): Int =
    if (i <= value) { value }
    else { throw new NoSuchElementException }

  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(i: Int) = i == value

  def remove(i: Int) = {
    if (value == i) EmptyIntDomain else this
  }

  def removeFrom(lb: Int) =
    if (lb > value) { this }
    else { EmptyIntDomain }

  def removeTo(ub: Int) =
    if (ub < value) { this }
    else { EmptyIntDomain }

  override def filter(f: Int => Boolean) =
    if (f(value)) { this }
    else { EmptyIntDomain }

  override def toString() = s"[$value]"

  def subsetOf(d: IntDomain) = d.present(value)

  lazy val toBitVector = BitVector.empty + value

  override def intersects(bv: BitVector) = {
    val part = value >> 6
    if (bv(value)) part else -1
  }

  def apply(i: Int) = if (i == 0) value else throw new IndexOutOfBoundsException

  override def intersects(bv: BitVector, part: Int) = bv(value)
  def bound = true
  override def isEmpty = false

  override def assign(v: Int) = throw new IllegalStateException("Trying to assign a singleton")
}
