package concrete.util

object Interval {
  def union(i0: Option[Interval], i1: Option[Interval]): Option[Interval] = {
    i0.map { i =>
      i1.map { i1 => i span i1 }
        .getOrElse(i)
    }
      .orElse(i1)
  }
}

case class Interval(val lb: Int, val ub: Int) {
  //assume(ub >= lb)
  val size: Int = math.max(0, ub - lb + 1)
  def contains(v: Int): Boolean = lb <= v && v <= ub

  def allValues: Range = lb to ub

  /**
   * [a, b] + [c, d] = [a + c, b + d]
   * [a, b] − [c, d] = [a − d, b − c]
   * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
   * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
   */

  def +(i: Interval): Interval = Interval(lb + i.lb, ub + i.ub)

  def +(v: Int): Interval = Interval(lb + v, ub + v)

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
    /** v / this **/
    if (this.contains(0)) throw new ArithmeticException
    Interval(
      math.min(ceilDiv(v, lb), ceilDiv(v, ub)),
      math.max(floorDiv(v, lb), floorDiv(v, ub)))
  }

  private def floorDiv(dividend: Int, divisor: Int): Int = {
    val roundedTowardsZeroQuotient = dividend / divisor;
    val dividedEvenly = (dividend % divisor) == 0;
    if (dividedEvenly) {
      roundedTowardsZeroQuotient;
    } else {
      // If they're of opposite sign then we rounded 
      // UP towards zero so we rem one. If they're of the same sign then 
      // we rounded DOWN towards zero, so we are done.

      if (divisor.signum == dividend.signum) {
        roundedTowardsZeroQuotient;
      } else {
        roundedTowardsZeroQuotient - 1;
      }
    }
  }

  private def ceilDiv(dividend: Int, divisor: Int): Int = {

    val roundedTowardsZeroQuotient = dividend / divisor;
    val dividedEvenly = (dividend % divisor) == 0;
    if (dividedEvenly) {
      roundedTowardsZeroQuotient;
    } else {
      // If they're of opposite sign then we rounded 
      // UP towards zero so we're done. If they're of the same sign then 
      // we rounded DOWN towards zero, so we need to add one.

      if (divisor.signum == dividend.signum) {
        roundedTowardsZeroQuotient + 1;
      } else {
        roundedTowardsZeroQuotient;
      }
    }
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
    math.max(lb, i.lb) <= math.min(ub, i.ub)
  }

  def span(i: Interval): Interval = Interval(math.min(lb, i.lb), math.max(ub, i.ub))

  def unary_- : Interval = Interval(-ub, -lb)

  def abs: Interval =
    if (ub < 0) { -this }
    else if (lb > 0) { this }
    else { Interval(0, math.max(-lb, ub)) }

}
