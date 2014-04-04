package concrete.util

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

object IntSet {
  def apply(domain: Int*): IntSet = {
    require(domain.size == 1 || domain.sliding(2).forall(p => p(0) < p(1)), "Only ordered domains are supported");

    domain match {
      case Seq() => EmptyIntSet
      case Seq(v) => new Singleton(0)
      case s: Seq[Int] if s.size == 1 + s.last - s.head => new IntervalSet(0, s.size - 1)
      case s: Seq[Int] => new BitVectorSet(s.size)
    }
  }

  def ofInterval(lb: Int, ub: Int): IntSet =
    if (lb > ub) { EmptyIntSet }
    else if (ub == lb) { new Singleton(lb) }
    else { new IntervalSet(lb, ub) }

  def ofBV(bv: BitVector, s: Int): IntSet = s match {
    case 0 => EmptyIntSet
    case 1 => new Singleton(bv.nextSetBit(0))
    case s: Int => {
      val lb = bv.nextSetBit(0)
      val ub = bv.lastSetBit
      if (ub - lb == s - 1) {
        new IntervalSet(lb, ub)
      } else {
        new BitVectorSet(bv, s)
      }
    }
  }

}

abstract class IntSet {
  def size: Int
  def copy: IntSet
  def first: Int
  def last: Int
  def next(i: Int): Int
  def prev(i: Int): Int
  def closestLeq(i: Int): Int
  def closestGeq(i: Int): Int
  def closestLt(i: Int): Int = prev(i)
  def closestGt(i: Int): Int = next(i)
  def present(i: Int): Boolean
  def remove(i: Int): IntSet
  def removeFrom(lb: Int): IntSet
  def removeTo(ub: Int): IntSet
  def filter(f: Int => Boolean): IntSet
  def subsetOf(d: IntSet): Boolean
  def toString(id: Indexer): String
  def toBitVector: BitVector
  def intersects(bv: BitVector): Int
  def intersects(bv: BitVector, part: Int): Boolean
  def bound: Boolean
  def isEmpty: Boolean

  def iterator = new Iterator[Int] {
    var current = IntSet.this.first
    def hasNext = current >= 0
    def next() = {
      val c = current
      current = IntSet.this.next(current)
      c
    }
  }

}
