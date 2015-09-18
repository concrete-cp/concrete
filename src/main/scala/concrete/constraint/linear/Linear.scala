package concrete.constraint.linear

import concrete.ParameterManager
import concrete.Variable
import concrete.constraint.Constraint
import concrete.ProblemState

object Linear {
  def apply(constant: Int, factors: Array[Int], scope: Array[Variable], mode: SumMode, pm: ParameterManager) = mode match {
    case SumLE => LinearLe(constant, factors, scope, false, pm)
    case SumLT => LinearLe(constant, factors, scope, true, pm)
    case SumEQ => LinearEq(constant, factors, scope)
    case SumNE => new LinearNe(constant, factors, scope)
  }

  def sortIntervals(factors: Array[Int], scope: Array[Variable]): (Array[Int], Array[Variable], Array[Int]) =
    (factors zip scope)
      .map { case (f, v) => (f, v, (v.initDomain.span.size * math.abs(f))) }
      .sortBy(-_._3)
      .unzip3

}

abstract class Linear(
    val constant: Int,
    val factors: Array[Int],
    scope: Array[Variable],
    val mode: SumMode) extends Constraint(scope) {

  require(factors.forall(_ != 0), this)
  require(factors.size == scope.size)

  private val checkSum: Function[Int, Boolean] =
    mode match {
      case SumLE => 0 <= _
      case SumLT => 0 < _
      case SumEQ => 0 == _
      case SumNE => 0 != _
    }

  def check(t: Array[Int]): Boolean = {
    var i = arity - 1
    var sum = 0
    while (i >= 0) {
      sum += t(i) * factors(i)
      i -= 1
    }
    checkSum(constant - sum)
  }

  def toString(operator: String) = {
    (scope zip factors).map { case (v, f) => f + "." + v }.mkString(" + ") + s" $operator $constant"
  }

  def toString(ps: ProblemState, operator: String) = {
    (scope zip factors).map { case (v, f) => f + "." + ps.dom(v) }.mkString(" + ") + s" $operator $constant"
  }

}
