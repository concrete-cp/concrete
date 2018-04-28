package concrete.constraint.semantic

import bitvectors.BitVector
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
    if (other(v)) {
      val tuple = new Array[Int](2)
      tuple(position) = value
      tuple(1 - position) = v
      Some(tuple)
    } else {
      None
    }
  }

  override def toString(ps: ProblemState) = s"$result =AC= |${v0.toString(ps)} - ${v1.toString(ps)}|";

  def advise(ps: ProblemState, pos: Int): Int = if (skip(ps)) -1 else ps.dom(v0).size + ps.dom(v1).size

  def simpleEvaluation = 2
}

final class AbsDiffConstBC(val result: Int, val v0: Variable, val v1: Variable)
  extends Constraint(Array(v0, v1)) with BC with ItvArrayFixPoint {

  val ops = Array(reviseX(_), reviseY(_))

  def init(ps: ProblemState): Outcome = ps

  def check(t: Array[Int]): Boolean = result == math.abs(t(0) - t(1))

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    fixPoint(ps)
  }

  override def toString(ps: ProblemState) =
    s"$result =BC= |${v0.toString(ps)} - ${v1.toString(ps)}|";

  def advise(ps: ProblemState, p: Int) = 5

  def simpleEvaluation = 1

  private def reviseX(doms: Array[Domain]) = {
    val x = doms(0).span
    val y = doms(1).span
    val diff = x - y
    if (!diff.abs.contains(result)) {
      None
    } else if (diff.lb >= 0) {
      Some(y + result)
    } else if (diff.ub <= 0) {
      Some(y - result)
    } else {
      Some((y + result).span(y - result))
    }
  }

  private def reviseY(doms: Array[Domain]) = {
    val x = doms(0).span
    val y = doms(1).span
    val diff = x - y
    if (!diff.abs.contains(result)) {
      None
    } else if (diff.lb >= 0) {
      Some(x - result)
    } else if (diff.ub <= 0) {
      Some(x + result)
    } else {
      Some((x + result).span(x - result))
    }
  }
}
