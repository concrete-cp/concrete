package concrete.constraint.semantic

import concrete._
import concrete.constraint._


final class AbsDiffConstAC(val result: Int, val v0: Variable, val v1: Variable)
  extends Constraint(Array(v0, v1)) with BCCompanion with Residues {

  def skipIntervals = false

  def check(t: Array[Int]): Boolean = result == math.abs(t(0) - t(1))

  override def findSupport(doms: Array[Domain], position: Int, value: Int): Option[Array[Int]] = {
    val other = doms(1 - position)

    support(value, value + result, other, position) orElse support(value, value - result, other, position)
    //    Seq(value + result, value - result).find(
    //      v => other.present(v)).map { v =>
    //
    //        val tuple = new Array[Int](2)
    //        tuple(position) = value
    //        tuple(1 - position) = v
    //        tuple
    //      }

  }

  def support(value: Int, v: Int, other: Domain, position: Int): Option[Array[Int]] = {
    if (other.present(v)) {
      val tuple = new Array[Int](2)
      tuple(position) = value
      tuple(1 - position) = v
      Some(tuple)
    } else {
      None
    }
  }

  override def toString(ps: ProblemState) = s"$result =AC= |${v0.toString(ps)} - ${v1.toString(ps)}|";

  def advise(ps: ProblemState, event: Event, pos: Int): Int = if (skip(ps)) -1 else ps.dom(v0).size + ps.dom(v1).size

  def simpleEvaluation = 2
}

final class AbsDiffConstBC(val result: Int, val v0: Variable, val v1: Variable)
  extends Constraint(Array(v0, v1)) with BC {

  def init(ps: ProblemState): Outcome = ps

  def check(t: Array[Int]): Boolean = result == math.abs(t(0) - t(1))

  override def shave(ps: ProblemState): Outcome = {
    val d0 = ps.dom(v0)
    val i0 = d0.span
    val d1 = ps.dom(v1)
    val i1 = d1.span

    val diff = i0 - i1

    if (!diff.abs.contains(result)) {
      Contradiction(scope)
    } else if (diff.lb >= 0) {
      ps.updateDom(v0, d0 & (i1 + result))
        .updateDom(v1, d1 & (i0 - result))
    } else if (diff.ub <= 0) {
      ps.updateDom(v0, d0 & (i1 - result))
        .updateDom(v1, d1 & (i0 + result))
    } else {
      ps.updateDom(v0, d0 & ((i1 + result) span (i1 - result)))
        .updateDom(v1, d1 & ((i0 - result) span (i0 + result)))
    }

  }

  override def toString(ps: ProblemState) =
    s"$result =BC= |${v0.toString(ps)} - ${v1.toString(ps)}|";

  def advise(ps: ProblemState, p: Int) = 5

  def simpleEvaluation = 1
}
