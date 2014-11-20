package concrete

import concrete.util.BitVector
import scala.collection.mutable.HashMap

object Singleton {
  val cache = new HashMap[Int, Singleton]
  def apply(v: Int) = cache.getOrElseUpdate(v, new Singleton(v))
}

final class Singleton(val index: Int) extends IntDomain {

  require(index >= 0)

  def length = 1

  override def head = index

  override def last = index

  def next(i: Int) = if (i < index) index else -1

  def prev(i: Int) = if (i > index) index else -1

  def prevOrEq(i: Int): Int =
    if (i >= index) { index }
    else { -1 }

  def nextOrEq(i: Int): Int =
    if (i <= index) { index }
    else { -1 }


  /**
   * @param index
   *            index to test
   * @return true iff index is present
   */
  def present(i: Int) = i == index

  def remove(i: Int) = {
    assert(index == i)
    EmptyIntDomain
  }

  def removeFrom(lb: Int) =
    if (lb > index) { this }
    else { EmptyIntDomain }

  def removeTo(ub: Int) =
    if (ub < index) { this }
    else { EmptyIntDomain }

  override def filter(f: Int => Boolean) =
    if (f(index)) { this }
    else { EmptyIntDomain }

  override def toString() = s"[$index]"

  def subsetOf(d: IntDomain) = d.present(index)

  lazy val toBitVector = BitVector.empty + index

  def intersects(bv: BitVector) = {
    val part = index >> 6
    if (bv(index)) part else -1
  }

  def intersects(bv: BitVector, part: Int) = bv(index)
  def bound = true
  override def isEmpty = false
}
