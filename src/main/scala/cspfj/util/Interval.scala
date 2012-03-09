package cspfj.problem

case class Interval(val a: Int, val b: Int) {
  // require(ub >= lb)
  val size = math.max(0, b - a + 1)
  def in(v: Int) = {
    a <= v && v <= b
  }
  def allValues = a to b
  def lb = a
  def ub = b

  /**
   * [a, b] + [c, d] = [a + c, b + d]
   * [a, b] − [c, d] = [a − d, b − c]
   * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
   * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
   */

  def +(i: Interval) = {
    val Interval(c, d) = i
    Interval(a + c, b + d)
  }

  def +(v: Int) = Interval(a + v, b + v)

  def -(i: Interval) = {
    val Interval(c, d) = i
    Interval(a - d, b - c)
  }

  def -(v: Int) = this + -v

  def *(i: Interval) = {
    val Interval(c, d) = i
    Interval(List(a * c, a * d, b * c, b * d).min, List(a * c, a * d, b * c, b * d).max)
  }

  def *(i: Int) = {
    Interval(math.min(a * i, b * i), math.max(a * i, b * i))
  }

  def /(i: Interval) = {
    if (i.in(0)) throw new ArithmeticException
    val Interval(c, d) = i
    Interval(List(a / c, a / d, b / c, b / d).min, List(a / c, a / d, b / c, b / d).max)
  }

  def /(v: Int) = {
    if (v == 0) throw new ArithmeticException
    Interval(math.min(a / v, b / v), math.max(a / v, b / v))
  }

}