package concrete.util

final case class Interval(val lb: Int, val ub: Int) {
  assume(ub >= lb)
  val size = ub - lb + 1
  def contains(v: Int) = lb <= v && v <= ub

  def allValues = lb to ub

  /**
   * [a, b] + [c, d] = [a + c, b + d]
   * [a, b] − [c, d] = [a − d, b − c]
   * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
   * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
   */

  def +(i: Interval) = Interval(lb + i.lb, ub + i.ub)

  def +(v: Int) = Interval(lb + v, ub + v)

  def -(i: Interval) = Interval(lb - i.ub, ub - i.lb)

  def -(v: Int) = this + -v

  def *(i: Interval) = {
    val Interval(c, d) = i
    Interval(List(lb * c, lb * d, ub * c, ub * d).min, List(lb * c, lb * d, ub * c, ub * d).max)
  }

  def *(v: Int) = {
    Interval(math.min(lb * v, ub * v), math.max(lb * v, ub * v))
  }

  def /(i: Interval) = {
    if (i.contains(0)) throw new ArithmeticException
    val Interval(c, d) = i
    Interval(
      List(ceilDiv(lb, c), ceilDiv(lb, d), ceilDiv(ub, c), ceilDiv(ub, d)).min,
      List(floorDiv(lb, c), floorDiv(lb, d), floorDiv(ub, c), floorDiv(ub, d)).max)
  }

  def /(v: Int) = {
    if (v >= 0) {
      Interval(ceilDiv(lb, v), floorDiv(ub, v))
    } else {
      Interval(ceilDiv(ub, v), floorDiv(lb, v))
    }

    //    if (l < u) Interval(l, u) else Interval(u, l)
  }

  def floorDiv(dividend: Int, divisor: Int) = {
    val roundedTowardsZeroQuotient = dividend / divisor;
    val dividedEvenly = (dividend % divisor) == 0;
    if (dividedEvenly) {
      roundedTowardsZeroQuotient;
    } else {
      // At this point we know that 
      // dividend was not zero (because there would have been no remainder)
      // Therefore both are non-zero.  Either they are of the same sign, 
      // or opposite signs. If they're of opposite sign then we rounded 
      // UP towards zero so we rem one. If they're of the same sign then 
      // we rounded DOWN towards zero, so we are done.

      val wasRoundedDown = ((divisor > 0) == (dividend > 0));
      if (wasRoundedDown) {
        roundedTowardsZeroQuotient;
      } else {
        roundedTowardsZeroQuotient - 1;
      }
    }
  }

  def ceilDiv(dividend: Int, divisor: Int) = {

    val roundedTowardsZeroQuotient = dividend / divisor;
    val dividedEvenly = (dividend % divisor) == 0;
    if (dividedEvenly) {
      roundedTowardsZeroQuotient;
    } else {
      // At this point we know that 
      // dividend was not zero (because there would have been no remainder)
      // Therefore both are non-zero.  Either they are of the same sign, 
      // or opposite signs. If they're of opposite sign then we rounded 
      // UP towards zero so we're done. If they're of the same sign then 
      // we rounded DOWN towards zero, so we need to add one.

      val wasRoundedDown = ((divisor > 0) == (dividend > 0));
      if (wasRoundedDown) {
        roundedTowardsZeroQuotient + 1;
      } else {
        roundedTowardsZeroQuotient;
      }
    }
  }

  def intersect(i: Interval) = {
    val l = math.max(lb, i.lb)
    val u = math.min(ub, i.ub)
    if (l <= u) { Some(Interval(l, u)) }
    else { None }
  }

  def union(i: Interval) = Interval(math.min(lb, i.lb), math.max(ub, i.ub))

  def negate = Interval(-ub, -lb)

  def abs =
    if (ub < 0) { negate }
    else if (lb > 0) { this }
    else { Interval(0, math.max(-lb, ub)) }

}
