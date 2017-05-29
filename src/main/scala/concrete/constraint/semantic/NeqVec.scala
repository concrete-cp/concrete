package concrete.constraint.semantic

;

import concrete._
import concrete.constraint.Constraint

import scala.annotation.tailrec

final class NeqVec(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) {

  require(x.length == y.length)

  val zip = x.zip(y)
  val size = arity / 2
  val simpleEvaluation = 2
  private val NONE = -2
  private val TWOP = -1

  def init(ps: ProblemState) = ps

  def check(t: Array[Int]) = {
    t.view.splitAt(size).zipped.exists(_ != _)
  }

  def revise(ps: ProblemState): Outcome = {
    val p = singleFreeVariable(ps)

    if (p == TWOP) {
      ps
    } else if (p == NONE) {
      if ((0 until size).forall(p => ps.dom(scope(p)).singleValue == ps.dom(scope(p + size)).singleValue)) {
        Contradiction(scope)
      } else {
        ps.entail(this)
      }
    } else if (p < size) {
      ps.removeIfPresent(scope(p), ps.dom(scope(p + size)).head)
    } else {
      ps.removeIfPresent(scope(p), ps.dom(scope(p - size)).head)
    }
  }

  override def toString(ps: ProblemState) =
    x.map(_.toString(ps)).mkString("(", ", ", ")") + " /= " + y.map(_.toString(ps)).mkString("(", ", ", ")")

  def advise(ps: ProblemState, event: Event, p: Int) = if (event <= Assignment) arity else -1

  @tailrec
  private def singleFreeVariable(
                                  ps: ProblemState,
                                  i: Int = 0,
                                  single: Int = NONE): Int = {

    if (i >= arity) {
      single
    } else if (!ps.dom(scope(i)).isAssigned) {
      if (single < 0) {
        singleFreeVariable(ps, i + 1, i)
      } else {
        TWOP
      }
    } else {
      singleFreeVariable(ps, i + 1, single)
    }
  }
}
