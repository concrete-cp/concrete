package cspfj.problem
import cspfj.util.BitVector

object IntSet {
  def factory(domain: Seq[Int]): IntSet = {
    require(domain.size == 1 || domain.sliding(2).forall(p => p(0) < p(1)), "Only ordered domains are supported");

    if (domain.size == 1 + domain.last - domain.head)
      new IntervalDomain(0, domain.size - 1)
    else
      new BitVectorDomain(domain.size)
  }
}

trait IntSet {
  def size: Int
  def copy: IntSet
  def first: Int
  def last: Int
  def next(i: Int): Int
  def prev(i: Int): Int
  def closestLeq(i: Int): Int
  def closestGeq(i: Int): Int
  def present(i: Int): Boolean
  def setSingle(i: Int): IntSet
  def remove(i: Int): IntSet
  def removeFrom(lb: Int): IntSet
  def removeTo(ub: Int): IntSet
  def subsetOf(d: IntSet): Boolean
  def toString(id: Indexer): String
  def toBitVector: BitVector
  def intersects(bv: BitVector): Int
  def intersects(bv: BitVector, part: Int): Boolean

  private def indices(i: Int): Stream[Int] =
    if (i < 0) Stream.empty
    else i #:: indices(next(i))

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