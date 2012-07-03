package cspfj.util

object IntSet {
  def apply(domain: Int*): IntSet = {
    require(domain.size == 1 || domain.sliding(2).forall(p => p(0) < p(1)), "Only ordered domains are supported");

    domain match {
      case Seq() => EmptyIntSet
      case Seq(v) => new Singleton(0)
      case s if domain.size == 1 + domain.last - domain.head => new IntervalSet(0, domain.size - 1)
      //case _ => new IntervalSet(0, domain.size - 1)
      case _ => new BitVectorSet(domain.size)
    }
  }

  def ofInterval(lb: Int, ub: Int): IntSet =
    if (lb > ub) EmptyIntSet
    else if (ub == lb) new Singleton(lb)
    else new IntervalSet(lb, ub)

  def ofBV(bv: BitVector, s: Int): IntSet = s match {
    case 0 => EmptyIntSet
    case 1 => new Singleton(bv.nextSetBit(0))
    case s => {
      val lb = bv.nextSetBit(0)
      val ub = bv.lastSetBit
      if (lb - ub == s - 1) new IntervalSet(lb, ub)
      else new BitVectorSet(bv, s)
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