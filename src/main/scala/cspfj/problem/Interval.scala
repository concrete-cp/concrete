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
   *
   */

  def +(i: Interval) = {
    val Interval(c, d) = i
    Interval(a + c, b + d)
  }

  def -(i: Interval) = {
    val Interval(c, d) = i
    Interval(a - d, b - c)
  }

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

  def /(i: Int) = {
    if (i > 0) {
      val a = this.a / i + (if (this.a % i != 0) 1 else 0)
      val b = this.b / i
      Interval(a, b)
    } else {
      val a = this.b / i - (if (this.b % i != 0) 1 else 0)
      val b = this.a / i
      Interval(a, b)
    }

    //Interval(math.ceil(min).toInt, math.floor(max).toInt)
  }

}