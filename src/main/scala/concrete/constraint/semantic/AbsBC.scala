package concrete
package constraint
package semantic

;

import bitvectors.BitVector
import concrete.util.Interval

final class AbsBC(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0)) with BC
  with FixPoint with ItvArrayFixPoint {

  val ops = Array(reviseResult(_), reviseV0(_))

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) == math.abs(t(1))

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {
    fixPoint(ps)
  }

  override def toString(ps: ProblemState) = s"${result.toString(ps)} =BC= |${v0.toString(ps)}|"

  def advise(ps: ProblemState, p: Int) = 6

  def simpleEvaluation = 1

  private def reviseResult(doms: Array[Domain]): Option[Interval] = Some(doms(1).span.abs)

  private def reviseV0(doms: Array[Domain]): Option[Interval] = {
    val ri = doms(0).span
    Some(ri.span(-ri))
  }
}
