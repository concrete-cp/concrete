package concrete.constraint.semantic;

import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.BC
import concrete.constraint.Constraint

/**
 * Contrainte V0 = V1 * V2.
 *
 * @author vion
 *
 */
final class MulBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC {

  def check(t: Array[Int]) = t(0) == (t(1) * t(2));

  def shave(ps: ProblemState): Outcome = {

    val rspan = ps.span(result)
    val v0span = ps.span(v0)
    val v1span = ps.span(v1)

    var ch = ps.shaveDom(result, v0span * v1span)

    if (!v1span.contains(0)) {
      ch = ch.shaveDom(v0, rspan / v1span)
    }

    if (!v0span.contains(0)) {
      ch = ch.shaveDom(v1, rspan / v0span)
    }

    ch
  }

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} =BC= ${v0.toString(ps)} * ${v1.toString(ps)}"

  def advise(ps: ProblemState, pos: Int) = 4

  val simpleEvaluation = 2
}
