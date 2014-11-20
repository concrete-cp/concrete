package concrete

import concrete.util.BitVector
import scala.collection.SortedSet
import concrete.util.Interval

object EmptyIntDomain extends IntDomain {
  def length = 0
  override def head = throw new NoSuchElementException
  override def last = throw new NoSuchElementException
  def next(i: Int) = throw new NoSuchElementException
  def prev(i: Int) = throw new NoSuchElementException
  def prevOrEq(i: Int) = throw new NoSuchElementException
  def nextOrEq(i: Int) = throw new NoSuchElementException
  def present(i: Int) = false
  def remove(i: Int) = this
  def removeFrom(lb: Int) = this
  def removeTo(ub: Int) = this
  override def filter(f: Int => Boolean) = this
  def setSingle(i: Int) = throw new AssertionError
  def subsetOf(d: IntDomain) = true
  override def toString = "[]"
  val toBitVector = BitVector.empty
  def intersects(bv: BitVector) = -1
  def intersects(bv: BitVector, part: Int) = false
  def bound = throw new IllegalStateException
  override def isEmpty = true
}

object IntDomain {
  //  def apply(size: Int): IntDomain = {
  //    size match {
  //      case 0 => EmptyIntDomain
  //      case 1 => new Singleton(0)
  //      case s => new IntervalDomain(0, s - 1)
  //    }
  //  }

  def ofInterval(lb: Int, ub: Int): IntDomain =
    if (lb > ub) { EmptyIntDomain }
    else if (ub == lb) { new Singleton(lb) }
    else { new IntervalDomain(lb, ub) }

  def ofBV(offset: Int, bv: BitVector, s: Int): IntDomain = s match {
    case 0 => EmptyIntDomain
    case 1 => new Singleton(offset + bv.nextSetBit(0))
    case s: Int => {
      val lb = bv.nextSetBit(0)
      val ub = bv.lastSetBit
      if (ub - lb == s - 1) {
        new IntervalDomain(offset + lb, offset + ub)
      } else {
        new BitVectorDomain(offset, bv, s)
      }
    }
  }

  def apply(d: SortedSet[Int]): IntDomain = {
    if (1 + d.last - d.head == d.size) ofInterval(d.head, d.last)
    else {
      val offset = d.head
      ofBV(offset, BitVector(d.map(_ - offset)), d.size)
    }
  }

  def apply(r: Range): IntDomain =
    if (r.step == 1 && r.size > 1) {
      ofInterval(r.start, r.last)
    } else {
      apply(r: _*)
    }

  def apply(i: Interval): IntDomain = ofInterval(i.lb, i.ub)

  @annotation.varargs
  def apply(d: Int*): IntDomain = apply(SortedSet[Int]() ++ d)

}

abstract class IntDomain extends Domain {

  def iterator = new Iterator[Int] {
    var current = IntDomain.this.head
    var end = false
    def hasNext = !end
    def next() = {
      val c = current
      if (current == last) {
        end = true
      } else {
        current = IntDomain.this.next(current)
      }
      c
      //} else Iterator.empty.next()

    }
  }

}
