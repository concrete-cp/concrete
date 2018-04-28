package concrete.constraint.semantic

import bitvectors.BitVector
import concrete.constraint.{BC, Constraint, ItvArrayFixPoint}
import concrete.util.Interval
import concrete.{Domain, Outcome, ProblemState, Variable}

final class AbsDiffBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC with ItvArrayFixPoint {

  val ops = Array(reviseResult(_), reviseV0(_), reviseV1(_))

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) == math.abs(t(1) - t(2))

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    fixPoint(ps)
  }

  override def toString(ps: ProblemState): String =
    s"${result.toString(ps)} =BC= |${v0.toString(ps)} - ${v1.toString(ps)}|"

  def advise(ps: ProblemState, pos: Int): Int = 5

  def simpleEvaluation = 2

  private def reviseResult(data: Array[Domain]) = {
    Some((data(1).span - data(2).span).abs)
  }

  private def reviseV0(data: Array[Domain]) = {
    val result = data(0).span
    val v0 = data(1).span
    val v1 = data(2).span
    Interval.unionSpan(v0.intersect(v1 - result), v0.intersect(v1 + result))
  }

  private def reviseV1(data: Array[Domain]) = {
    val result = data(0).span
    val v0 = data(1).span
    val v1 = data(2).span
    Interval.unionSpan(v1.intersect(v0 - result), v1.intersect(v0 + result))
  }
}