package concrete

import bitvectors.{BitVector, BitVectorBuilder}
import concrete.util.Interval
import cspom.util._

import scala.collection.immutable.TreeSet



object IntDomain {
  val DISPLAYED_VALUES = 5

  private val TREE_SET_THRESHOLD = 0.01

  def ofInterval(i: Interval): IntDomain = i.size match {
    case 0 => EmptyIntDomain
    case 1 => Singleton(i.lb)
    case _ => new IntervalDomain(i)
  }

  def apply(d: RangeSet[Infinitable]): IntDomain = {
    val cspom.util.Interval(Finite(offset), Finite(ub)) = d.span

    if (d.isConvex) {
      ofInterval(offset, ub)
    } else {
      val asSet = new ContiguousIntRangeSet(d)
      val size = asSet.size
      if (size.toDouble / (ub - offset + 1) < TREE_SET_THRESHOLD) {
        val builder = TreeSet.newBuilder[Int]
        builder ++= asSet
        new TreeSetDomain(builder.result())
      } else {
        val bvb = new BitVectorBuilder(ub - offset + 1)
        for (FiniteIntInterval(l, u) <- d.contents) {
          bvb.set(l - offset, u - offset + 1)
        }
        new BitVectorDomain(offset, bvb.result(), size)
      }
    }
//
//    offset - ub - 1 match {
//      case 0 => EmptyIntDomain
//      case 1 => Singleton(offset)
//      case _ =>
//        if (d.isConvex) {
//          ofInterval()
//        }
//        val bvb = new BitVectorBuilder(ub - offset + 1)
//        var s = 0
//        for (FiniteIntInterval(l, u) <- d.ranges) {
//          bvb.set(l - offset, u - offset + 1)
//          s += u - l + 1
//        }
//        val bv = bvb.result()
//        ofBitVector(offset, bv, s)
//    }
  }

  def apply(r: Range): IntDomain =
    if (r.step == 1) {
      ofInterval(r.start, r.last)
    } else {
      val offset = r.start
      ofBitVector(offset, BitVector(0 to r.end - offset by r.step), r.size)
    }

  def ofBitVector(offset: Int, bv: BitVector, s: Int): IntDomain = s match {
    case 0 => EmptyIntDomain
    case 1 => Singleton(offset + bv.nextSetBit(0))
    case s: Int => {
      val lb = bv.nextSetBit(0)
      val ub = bv.lastSetBit
      assert(s == bv.cardinality)
      if (ub - lb == s - 1) {
        new IntervalDomain(offset + lb, offset + ub)
      } else if (lb >= BitVector.WORD_SIZE) {
        new BitVectorDomain(offset + lb, bv.shift(-lb), s)
      } else {
        new BitVectorDomain(offset, bv, s)
      }
    }
  }

  def ofInterval(lb: Int, ub: Int): IntDomain =
    if (lb > ub) {
      EmptyIntDomain
    }
    else if (ub == lb) {
      Singleton(lb)
    }
    else {
      new IntervalDomain(lb, ub)
    }

  def apply(i: Interval): IntDomain = ofInterval(i.lb, i.ub)

  @annotation.varargs
  def ofSeq(d: Int*): IntDomain = ofTreeSet(TreeSet(d: _*))

  def ofTreeSet(set: TreeSet[Int]): IntDomain = {
    set.size match {
      case 0 => EmptyIntDomain
      case 1 => Singleton(set.head)
      case s: Int =>
        val lb = set.head
        val ub = set.last

        val span = ub - lb + 1
        if (span == s) {
          new IntervalDomain(lb, ub)
        } else if (s.toDouble / span < TREE_SET_THRESHOLD) {
          new TreeSetDomain(set)
        } else {
          val bvb = new BitVectorBuilder(span)
          for (i <- set) bvb += i - lb
          new BitVectorDomain(lb, bvb.result(), s)
        }
    }
  }

}

abstract class IntDomain extends Domain {

  final def assign(value: Int): Singleton = {
    assert(present(value), s"tried to assign to $value which is not present")
    assert(!isAssigned, s"domain is already assigned to $value")
    //if (present(value)) 
    Singleton(value)
    //else EmptyIntDomain
  }

  def |(value: Int): IntDomain

  def ++(values: Traversable[Int]): IntDomain = values.foldLeft(this)(_ | _)

  def removeItv(from: Int, to: Int): Domain = {
    val removed = removeFrom(from) | removeTo(to)
    if (removed.size == size) this else removed
  }

}
