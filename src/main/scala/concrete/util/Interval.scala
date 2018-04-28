package concrete
package util

import java.lang.Math.floorDiv
import Math.ceilDiv
import java.lang.Math.addExact
import java.lang.Math.multiplyExact

import math._

object Interval {
  def intersect(o1: Option[Interval], o2: Option[Interval]): Option[Interval] = {
    for (i1 <- o1; i2 <- o2; intersect <- i1.intersect(i2)) yield intersect
  }

  def unionSpan(i0: Option[Interval], i1: Option[Interval]): Option[Interval] = {
    i0.map { i =>
      i1.map { i1 => i span i1 }
        .getOrElse(i)
    }
      .orElse(i1)
  }

  /**
    * Will merge intervals if they are connected (in this case the second returned interval is None)
    */
  def realUnion(i0: Option[Interval], i1: Option[Interval]): (Option[Interval], Option[Interval]) = {
    if (i0.isDefined && i1.isDefined && i0.get.connected(i1.get)) {
      (Some(i0.get span i1.get), None)
    } else {
      (i0, i1)
    }
  }

  def realUnion(i0: Interval, i1: Interval): Either[Interval, (Interval, Interval)] = {
    if (i1 connected i0) Left(i0 span i1)
    else if (i0.lb < i1.lb) Right((i0, i1))
    else Right((i1, i0))
  }

  def option(lb: Int, ub: Int): Option[Interval] = {
    if (lb <= ub) {
      Some(Interval(lb, ub))
    } else {
      None
    }
  }
}

case class Interval(lb: Int, ub: Int) {
  assert(lb <= ub, s"Empty interval created: [$lb, $ub]")

  val size: Int = ub - lb + 1

  def allValues: Range = lb to ub

  /**
    * [a, b] + [c, d] = [a + c, b + d]
    * [a, b] − [c, d] = [a − d, b − c]
    * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
    * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
    */

  def +(i: Interval): Interval = {
    Interval(lb + i.lb, ub + i.ub)
  }

  def checkedAdd(i: Interval): Interval = {
    Interval(addExact(lb, i.lb), addExact(ub, i.ub))
  }

  def except(i: Int): Option[Either[Interval, (Interval, Interval)]] = {
    if (i < lb || i > ub) {
      Some(Left(this))
    } else if (i == lb) {
      if (i == ub) {
        None
      } else {
        Some(Left(Interval(lb + 1, ub)))
      }
    } else if (i == ub) {
      Some(Left(Interval(lb, ub - 1)))
    } else {
      Some(Right((Interval(lb, i - 1), Interval(i + 1, ub))))
    }
  }

  def shrink(i: Interval): Interval = Interval(lb - i.lb, ub - i.ub)

  def -(i: Interval): Interval = Interval(lb - i.ub, ub - i.lb)

  def -(v: Int): Interval = this + -v

  def +(v: Int): Interval = Interval(lb + v, ub + v)

  def *(i: Interval): Interval = {
    val Interval(c, d) = i

    val p1 = lb * c
    val p2 = lb * d
    val p3 = ub * c
    val p4 = ub * d

    val l = min(p1, min(p2, min(p3, p4)))
    val u = max(p1, max(p2, max(p3, p4)))

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

  def checkedMultiply(v: Int): Interval = {
    val lbv = multiplyExact(lb, v)
    val ubv = multiplyExact(ub, v)
    if (lbv < ubv) {
      Interval(lbv, ubv)
    } else {
      Interval(ubv, lbv)
    }
  }

  def sq: Interval = {
    val lb2 = lb * lb
    val ub2 = ub * ub
    if (contains(0)) {
      Interval(0, max(lb2, ub2))
    } else if (lb2 < ub2) {
      Interval(lb2, ub2)
    } else {
      Interval(ub2, lb2)
    }
  }

  def sqrt: Interval = {
    require(lb >= 0)
    val root = math.sqrt(ub).toInt
    Interval(-root, root)
  }

  def /(i: Interval): Option[Interval] = {
    if (i.contains(0)) throw new ArithmeticException
    val Interval(c, d) = i

    val a = lb
    val b = ub


    val l = min(ceilDiv(a, c), min(ceilDiv(b, c), min(ceilDiv(a, d), ceilDiv(b, d))))
//      java.lang.Math.toIntExact(round(ceil(
//        min(ac, min(bc, min(ad, bd))))))

    val u = max(floorDiv(a, c), max(floorDiv(b, c), max(floorDiv(a, d), floorDiv(b, d))))
//      java.lang.Math.toIntExact(round(floor(
//        max(ac, max(bc, max(ad, bd))))))

    Interval.option(l, u)
  }

  def /(v: Int): Option[Interval] = {
    val (l, u) =
    if (v >= 0) {
      (ceilDiv(lb, v), floorDiv(ub, v))
    } else {
      (ceilDiv(ub, v), floorDiv(lb, v))
    }

    Interval.option(l, u)

  }

  def /:(v: Int): Option[Interval] = {
    // v / this
    if (this.contains(0)) throw new ArithmeticException
    Interval.option(
      min(ceilDiv(v, lb), ceilDiv(v, ub)),
      max(floorDiv(v, lb), floorDiv(v, ub)))
  }

  def contains(v: Int): Boolean = lb <= v && v <= ub

  def fastIntersect(ilb: Int, iub: Int): Interval = {
    Interval(max(lb, ilb), min(ub, iub))
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
  }

  def from(lb: Int): Option[Interval] = {
    if (lb <= this.lb) {
      Some(this)
    } else if (lb > this.ub) {
      None
    } else {
      Some(Interval(lb, this.ub))
    }
  }

  def to(ub: Int): Option[Interval] = {
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

  def abs: Interval =
    if (ub < 0) {
      -this
    }
    else if (lb > 0) {
      this
    }
    else {
      Interval(0, math.max(-lb, ub))
    }

  def unary_- : Interval = Interval(-ub, -lb)

}
