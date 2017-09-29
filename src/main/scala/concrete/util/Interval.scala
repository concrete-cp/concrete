package concrete
package util

import java.lang.Math.floorDiv
import Math.ceilDiv

object Interval {
  def union(i0: Option[Interval], i1: Option[Interval]): Option[Interval] = {
    i0.map { i =>
      assert(i.nonEmpty)
      i1.map { i1 => i span i1 }
        .getOrElse(i)
    }
      .orElse(i1)
  }

  def realUnion(i0: Option[Interval], i1: Option[Interval]): Option[Either[Interval, (Interval, Interval)]] = {
    i0 match {
      case Some(i0) =>
        i1.map(realUnion(i0, _)).orElse(Some(Left(i0)))
      case None => i1.map(Left(_))
    }

  }

  def realUnion(i0: Interval, i1: Interval): Either[Interval, (Interval, Interval)] = {
    if (i1 connected i0) Left(i0 span i1)
    else if (i0.lb < i1.lb) Right((i0, i1))
    else Right((i1, i0))
  }

}

case class Interval(lb: Int, ub: Int) {
  //assume(ub >= lb)
  val size: Int = math.max(0, ub - lb + 1)
  def contains(v: Int): Boolean = lb <= v && v <= ub

  def isEmpty: Boolean = ub < lb
  def nonEmpty: Boolean = ub >= lb

  def allValues: Range = lb to ub

  /**
   * [a, b] + [c, d] = [a + c, b + d]
   * [a, b] − [c, d] = [a − d, b − c]
   * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
   * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
   */

  def +(i: Interval): Interval = Interval(lb + i.lb, ub + i.ub)

  def +(v: Int): Interval = Interval(lb + v, ub + v)

  def shrink(i: Interval): Interval = Interval(lb - i.lb, ub - i.ub)

  def -(i: Interval): Interval = Interval(lb - i.ub, ub - i.lb)

  def -(v: Int): Interval = this + -v

  def *(i: Interval): Interval = {
    val Interval(c, d) = i

    val p1 = lb * c
    val p2 = lb * d
    val p3 = ub * c
    val p4 = ub * d

    val l = math.min(p1, math.min(p2, math.min(p3, p4)))
    val u = math.max(p1, math.max(p2, math.max(p3, p4)))

    Interval(l, u)
  }

  def *(v: Int): Interval = {
    val lbv = lb * v
    val ubv = ub * v
    if (lbv < ubv) {
      Interval(lbv, ubv)
    } else {
      Interval(ubv, lbv)
    }
  }

  def sq(): Interval = {
    val lb2 = lb * lb
    val ub2 = ub * ub
    if (contains(0)) {
      Interval(0, math.max(lb2, ub2))
    } else {
      if (lb2 < ub2) {
        Interval(lb2, ub2)
      } else {
        Interval(ub2, lb2)
      }
    }
  }

  def sqrt(): Interval = {
    require(lb >= 0)
    val root = math.sqrt(ub).toInt
    Interval(-root, root)
  }

  def /(i: Interval): Interval = {
    if (i.contains(0)) throw new ArithmeticException
    val Interval(c, d) = i

    val l = math.min(
      ceilDiv(lb, c), math.min(ceilDiv(lb, d), math.min(ceilDiv(ub, c), ceilDiv(ub, d))))

    val u = math.max(
      floorDiv(lb, c), math.max(floorDiv(lb, d), math.max(floorDiv(ub, c), floorDiv(ub, d))))

    Interval(l, u)
  }

  def /(v: Int): Interval = {
    if (v >= 0) {
      Interval(ceilDiv(lb, v), floorDiv(ub, v))
    } else {
      Interval(ceilDiv(ub, v), floorDiv(lb, v))
    }

    //    if (l < u) Interval(l, u) else Interval(u, l)
  }

  def /:(v: Int): Interval = {
    // v / this 
    if (this.contains(0)) throw new ArithmeticException
    Interval(
      math.min(ceilDiv(v, lb), ceilDiv(v, ub)),
      math.max(floorDiv(v, lb), floorDiv(v, ub)))
  }

  def fastIntersect(ilb: Int, iub: Int): Interval = {
    Interval(math.max(lb, ilb), math.min(ub, iub))
  }

  def intersect(i: Interval): Option[Interval] = {
    if (i.lb <= lb) {
      if (i.ub < lb) {
        None
      } else if (i.ub < ub) {
        Some(Interval(lb, i.ub))
      } else {
        Some(this)
      }
    } else if (i.lb <= ub) {
      if (i.ub <= ub) {
        Some(i)
      } else {
        Some(Interval(i.lb, ub))
      }
    } else {
      None
    }
    //    val l = math.max(lb, i.lb)
    //    val u = math.min(ub, i.ub)
    //    if (l <= u) {
    //      if (l == lb && u == ub) {
    //        Some(this)
    //      } else {
    //        Some(Interval(l, u))
    //      }
    //    } else {
    //      None
    //    }
  }

  def shaveLb(lb: Int): Option[Interval] = {
    if (lb <= this.lb) {
      Some(this)
    } else if (lb > this.ub) {
      None
    } else {
      Some(Interval(lb, this.ub))
    }
  }

  def shaveUb(ub: Int): Option[Interval] = {
    if (ub >= this.ub) {
      Some(this)
    } else if (ub < this.lb) {
      None
    } else {
      Some(Interval(this.lb, ub))
    }
  }

  def intersects(i: Interval): Boolean = {
    lb <= i.ub && ub >= i.lb
    //    math.max(lb, i.lb) <= math.min(ub, i.ub)
  }

  def connected(i: Interval): Boolean = {
    val iAfterThis = i.lb > this.ub + 1

    !(iAfterThis || i.ub < this.lb - 1)
  }

  def subsetOf(i2: Interval): Boolean = {
    i2.lb <= lb && ub <= i2.ub
  }

  def span(i: Interval): Interval = Interval(math.min(lb, i.lb), math.max(ub, i.ub))

  def unary_- : Interval = Interval(-ub, -lb)

  def abs: Interval =
    if (ub < 0) { -this }
    else if (lb > 0) { this }
    else { Interval(0, math.max(-lb, ub)) }

}
