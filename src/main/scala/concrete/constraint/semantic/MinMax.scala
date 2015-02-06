package concrete.constraint.semantic

import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.Interval

abstract class MinMax(result: Variable, vars: Array[Variable], constant: Option[Int]) extends Constraint(result +: vars) {

  def advise(ps: ProblemState, pos: Int): Int = arity

  def in(dom: Iterator[Domain]): Interval = {
    val span = dom.map(_.span).reduce(_ span _)
    constant.map(c => Interval(c, c) span span).getOrElse(span)
  }

  def simpleEvaluation: Int = 2

}

final class Min(result: Variable, vars: Array[Variable], constant: Option[Int])
  extends MinMax(result, vars, constant) {

  def this(result: Variable, vars: Array[Variable]) = this(result, vars, None)
  def this(result: Variable, vars: Array[Variable], constant: Int) = this(result, vars, Some(constant))

  def revise(ps: ProblemState): Outcome = {
    val span = in(ps.domains(vars))

    ps.shaveDom(result, span).andThen {
      ps =>
        val minValue = ps.dom(result).head
        ps.updateAll(vars)(_.removeUntil(minValue))
    }

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).min
  }

  override def toString(ps: ProblemState) = ps.domains(vars).mkString(ps.dom(result) + " = min(", ", ", ")")
}

final class Max(result: Variable, vars: Array[Variable], constant: Option[Int])
  extends MinMax(result, vars, constant) {

  def this(result: Variable, vars: Array[Variable]) = this(result, vars, None)
  def this(result: Variable, vars: Array[Variable], constant: Int) = this(result, vars, Some(constant))

  def revise(ps: ProblemState): Outcome = {
    val span = in(ps.domains(vars))

    ps.shaveDom(result, span).andThen {
      ps =>
        val maxValue = ps.dom(result).last
        ps.updateAll(vars.iterator)(_.removeAfter(maxValue))
    }

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == (1 until arity).map(tuple).max
  }

  override def toString(ps: ProblemState) = ps.domains(vars).mkString(ps.dom(result) + " = min(", ", ", ")")
}