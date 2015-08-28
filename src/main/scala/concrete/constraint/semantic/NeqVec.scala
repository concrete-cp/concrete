package concrete.constraint.semantic;

import scala.annotation.tailrec

import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint

final class NeqVec(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) {

  require(x.length == y.length)

  def init(ps: ProblemState) = ps

  val zip = x.zip(y)

  val size = arity / 2

  def check(t: Array[Int]) = {
    t.view.splitAt(size).zipped.exists(_ != _)
  }

  def revise(ps: ProblemState): Outcome = {
    val p = singleFreeVariable(ps)

    if (p < 0) {
      ps
    } else if (p < size) {
      ps.remove(scope(p), ps.dom(scope(p + size)).head)
    } else {
      ps.remove(scope(p), ps.dom(scope(p - size)).head)
    }
  }

  @tailrec
  private def singleFreeVariable(
    ps: ProblemState,
    i: Int = 0,
    single: Int = -1): Int = {

    if (i >= arity) {
      single
    } else if (ps.dom(scope(i)).size > 1) {
      if (single < 0) {
        singleFreeVariable(ps, i + 1, i)
      } else {
        -1
      }
    } else {
      singleFreeVariable(ps, i + 1, single)
    }
  }

  override def toString(ps: ProblemState) =
    x.map(_.toString(ps)).mkString("(", ", ", ")") + " /= " + y.map(_.toString(ps)).mkString("(", ", ", ")")

  def advise(ps: ProblemState, p: Int) = arity

  val simpleEvaluation = 2
}
