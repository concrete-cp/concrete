package concrete.constraint.semantic;

import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.StatefulConstraint
import concrete.Domain
import cspom.UNSATException
import concrete.constraint.Removals

final class LexLeq(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y)
    with StatefulConstraint[(Int, Int)] with Removals {

  type State = (Int, Int)

  val n = x.length
  require(n == y.length)

  def check(t: Array[Int]) = check(t, 0)

  @annotation.tailrec
  private def check(t: Array[Int], i: Int): Boolean =
    i >= n ||
      t(i) < t(i + n) || (t(i) == t(i + n) && check(t, i + 1))

  def groundEq(x: Variable, y: Variable, ps: ProblemState): Boolean = {
    val xdom = ps.dom(x)
    xdom.size == 1 && {
      val ydom = ps.dom(y)
      ydom.size == 1 && xdom.singleValue == ydom.singleValue
    }
  }
  override def toString(ps: ProblemState) = {
    val (alpha, beta) = ps(this)
    s"$id: ${x.map(ps.dom).mkString("[", ", ", "]")} <= ${y.map(ps.dom).mkString("[", ", ", "]")} / alpha = $alpha, beta = $beta"
  }

  override def init(ps: ProblemState) = {

    var alpha = 0
    while (alpha < n && groundEq(x(alpha), y(alpha), ps)) {
      alpha += 1
    }

    if (alpha == n) {
      ps.updateState(id, (alpha, n + 1))
    } else {
      var i = alpha
      var beta = -1
      while (i != n && min(x(i), ps) <= max(y(i), ps)) {
        if (min(x(i), ps) == max(y(i), ps)) {
          if (beta == -1) beta = i
        } else {
          beta = -1
        }
        i += 1
      }
      if (i == n) {
        beta = n + 1
      } else if (beta == -1) {
        beta = i
      }

      if (alpha == beta) {
        Contradiction
      } else {
        reEstablishGAC(alpha, ps.updateState(id, (alpha, beta)))
      }
    }

  }

  def revise(ps: ProblemState, mod: List[Int]): Outcome = {
    val (x, y) = mod.span(_ < n)
    val r = reviseN(ps, (x ++ y.map(_ - n)).distinct)
    //    val out = r match {
    //      case Contradiction    => "Contradiction"
    //      case ns: ProblemState => if (ns eq ps) "NOP" else if (ns.domains eq ps.domains) ns(this) else toString(ns)
    //    }
    //    println(s"${toString(ps)}, $mod -> $out")
    r
  }

  private def reviseN(ps: ProblemState, mod: List[Int]): Outcome = mod match {
    case Nil => ps
    case head :: tail =>
      reEstablishGAC(head, ps).andThen(ps => reviseN(ps, tail))
  }

  private def min(v: Variable, ps: ProblemState) = ps.dom(v).head
  private def max(v: Variable, ps: ProblemState) = ps.dom(v).last

  /**
   * Triggered when min(x(i)) or max(y(i)) changes
   */
  private def reEstablishGAC(i: Int, ps: ProblemState): Outcome = {
    val (alpha, beta) = ps(this)

    if (i == alpha) {
      if (i + 1 == beta) {
        establishAC(x(i), y(i), true, ps)
      } else if (i + 1 < beta) {
        establishAC(x(i), y(i), false, ps).andThen {
          ps =>
            if (groundEq(x(i), y(i), ps)) {
              updateAlpha(alpha + 1, beta, ps)
            } else {
              ps
            }
        }
      } else {
        ps
      }
    } else if (alpha < i && i < beta) {
      val minxi = min(x(i), ps)
      val maxyi = max(y(i), ps)
      if ((i == beta - 1 && minxi == maxyi) || minxi > maxyi) {
        updateBeta(i - 1, alpha, ps)
      } else {
        ps
      }
    } else {
      ps
    }
  }

  private def establishAC(x: Variable, y: Variable, strict: Boolean, ps: ProblemState): Outcome = {
    if (strict) {
      ps.removeTo(y, min(x, ps))
        .removeFrom(x, max(y, ps))
    } else {
      ps.removeUntil(y, min(x, ps))
        .removeAfter(x, max(y, ps))
    }
  }

  private def updateAlpha(alpha: Int, beta: Int, ps: ProblemState): Outcome = {
    if (alpha == n) {
      ps.updateState(id, (alpha, beta))
    } else if (alpha == beta) {
      Contradiction
    } else if (groundEq(x(alpha), y(alpha), ps)) {
      updateAlpha(alpha + 1, beta, ps)
    } else {
      reEstablishGAC(alpha, ps.updateState(id, (alpha, beta)))
    }
  }

  private def updateBeta(i: Int, alpha: Int, ps: ProblemState): Outcome = {
    val beta = i + 1
    if (alpha == beta) {
      Contradiction
    } else if (min(x(i), ps) < max(y(i), ps)) {
      if (i == alpha) {
        establishAC(x(i), y(i), true, ps.updateState(id, (alpha, beta)))
      } else {
        ps.updateState(id, (alpha, beta))
      }
    } else {
      updateBeta(i - 1, alpha, ps)
    }

  }

  def getEvaluation(ps: ProblemState) = n
  def simpleEvaluation = 2

}
