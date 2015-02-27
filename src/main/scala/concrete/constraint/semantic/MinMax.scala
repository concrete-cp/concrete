package concrete.constraint.semantic

import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.BC
import concrete.constraint.Constraint
import concrete.util.Interval

abstract class MinMax(result: Variable, vars: Array[Variable]) extends Constraint(result +: vars) with BC {

  def advise(ps: ProblemState, pos: Int): Int = arity

  def simpleEvaluation: Int = 2

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} = ${this.getClass.getSimpleName}${vars.map(_.toString(ps)).mkString("(", ", ", ")")}"

}

final class Min(result: Variable, vars: Array[Variable])
  extends MinMax(result, vars) {

  def shave(ps: ProblemState): Outcome = {

    var lb = Int.MaxValue
    var ub = Int.MaxValue
    for (v <- vars) {
      val dom = ps.dom(v)
      lb = math.min(lb, dom.head)
      ub = math.min(ub, dom.last)
    }

    ps.shaveDom(result, lb, ub).andThen {
      ps =>
        val minValue = ps.dom(result).head
        ps.updateAll(vars)(_.removeUntil(minValue))
    }

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).min
  }

}

final class Max(result: Variable, vars: Array[Variable])
  extends MinMax(result, vars) {

  def shave(ps: ProblemState): Outcome = {

    var lb = Int.MinValue
    var ub = Int.MinValue
    for (v <- vars) {
      val dom = ps.dom(v)
      lb = math.max(lb, dom.head)
      ub = math.max(ub, dom.last)
    }

    ps.shaveDom(result, lb, ub).andThen {
      ps =>
        val maxValue = ps.dom(result).last

        ps.updateAll(vars)(_.removeAfter(maxValue))
    }

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == tuple.view.slice(1, arity).max
  }

}