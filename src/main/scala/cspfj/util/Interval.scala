package cspfj.util

final case class Interval(val lb: Int, val ub: Int) {
  require(ub >= lb)
  val size = ub - lb + 1
  def in(v: Int) = {
    lb <= v && v <= ub
  }
  def allValues = lb to ub

  /**
   * [a, b] + [c, d] = [a + c, b + d]
   * [a, b] − [c, d] = [a − d, b − c]
   * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
   * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
   */

  def +(i: Interval) = Interval(lb + i.lb, ub + i.ub)

  def +(v: Int) = Interval(lb + v, ub + v)

  def -(i: Interval) = this + i.opp

  def -(v: Int) = this + -v

  def *(i: Interval) = {
    val Interval(c, d) = i
    Interval(List(lb * c, lb * d, ub * c, ub * d).min, List(lb * c, lb * d, ub * c, ub * d).max)
  }

  def *(v: Int) = {
    Interval(math.min(lb * v, ub * v), math.max(lb * v, ub * v))
  }

  def /(i: Interval) = {
    if (i.in(0)) throw new ArithmeticException
    val Interval(c, d) = i
    Interval(List(lb / c, lb / d, ub / c, ub / d).min, List(lb / c, lb / d, ub / c, ub / d).max)
  }

  def /(v: Int) = {
    if (v == 0) throw new ArithmeticException
    Interval(math.min(lb / v, ub / v), math.max(lb / v, ub / v))
  }

  def intersect(i: Interval) = {
    val l = math.max(lb, i.lb)
    val u = math.min(ub, i.ub)
    if (l <= u) Some(Interval(l, u))
    else None
  }

  def union(i: Interval) = Interval(math.min(lb, i.lb), math.max(ub, i.ub))

  lazy val opp = Interval(-ub, -lb)

  lazy val abs =
    if (ub < 0) opp
    else if (lb > 0) this
    else Interval(0, math.max(-lb, ub))

}