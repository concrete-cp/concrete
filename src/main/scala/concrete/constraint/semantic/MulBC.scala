package concrete.constraint.semantic

;

import bitvectors.BitVector
import concrete.{Domain, Outcome, ProblemState, Variable}
import concrete.constraint.{BC, Constraint, ItvArrayFixPoint}
import concrete.util.Interval

/**
  * Constraint result = v0 * v1
  *
  * @author vion
  *
  */
final class MulBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC with ItvArrayFixPoint {
  val simpleEvaluation = 2
  val ops: Array[Array[Domain] => Option[Interval]] = Array(reviseResult(_), reviseV0(_), reviseV1(_))

  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) == (t(1) * t(2))

  def revise(ps: ProblemState, mod: BitVector): Outcome = fixPoint(ps)

  override def toString(ps: ProblemState) =
    s"${result.toString(ps)} =BC= ${v0.toString(ps)} * ${v1.toString(ps)}"

  def advise(ps: ProblemState, pos: Int) = 4

  //  override def shave(ps: ProblemState): Outcome = {
  //
  //    val rspan = ps.span(result)
  //    val v0span = ps.span(v0)
  //    val v1span = ps.span(v1)
  //
  //    var ch = ps.shaveDom(result, v0span * v1span)
  //
  //    if (!v1span.contains(0)) {
  //      ch = ch.shaveDom(v0, rspan / v1span)
  //    }
  //
  //    if (!v0span.contains(0)) {
  //      ch = ch.shaveDom(v1, rspan / v0span)
  //    }
  //
  //    ch
  //  }

  private def reviseResult(doms: Array[Domain]): Option[Interval] =
    Some(doms(1).span * doms(2).span)

  private def reviseV0(doms: Array[Domain]): Option[Interval] = {
    // v0 = r / v1

    if (doms(2).contains(0) && doms(0).contains(0)) {
      Some(doms(1).span)
    } else {
      // r = v0 * v1 => if 0 notin r, then 0 notin v0 and 0 notin v1
      // therefore 0 in v1 can be ignored
      val (neg, pos) = Div.splitRealDiv(doms(0).span, doms(2).span)
      Interval.unionSpan(neg, pos)
    }
  }

  private def reviseV1(doms: Array[Domain]): Option[Interval] = {
    if (doms(1).contains(0) && doms(0).contains(0)) {
      Some(doms(2).span)
    } else {
      val (neg, pos) = Div.splitRealDiv(doms(0).span, doms(1).span)
      Interval.unionSpan(neg, pos)
    }
  }
}
