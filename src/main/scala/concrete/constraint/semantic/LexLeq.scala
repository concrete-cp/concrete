package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.Variable
import concrete.constraint.TupleEnumerator
import concrete.UNSATException
import concrete.UNSATObject
import concrete.Domain
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome

final class LexLeq(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) {

  val size = x.length
  require(size == y.length)

  private def groundEq(ps: ProblemState, i: Int) = {
    val domX = ps.dom(scope(i))
    val domY = ps.dom(scope(i + size))
    domX.size == 1 && domY.size == 1 && domX.singleValue == domY.singleValue
  }

  def check(t: Array[Int]) = ???

  private def notAlwaysLt(ps: ProblemState, i: Int) = ps.dom(scope(i)).head <= ps.dom(scope(i + size)).last

  private def alwaysLeq(ps: ProblemState, i: Int) = ps.dom(scope(i)).last <= ps.dom(scope(i + size)).head

  private def alwaysLt(ps: ProblemState, i: Int) = ps.dom(scope(i)).last < ps.dom(scope(i + size)).head

  private def checkLex(ps: ProblemState, i: Int) = {
    if (i == size - 1) alwaysLeq(ps, i)
    else alwaysLt(ps, i)
  }

  var alpha = -1
  var beta = -1

  def revise(ps: ProblemState): Outcome = {
    var i = 0
    while (i < size && groundEq(ps, i)) i += 1

    if (i == size) {
      ps
    } else {
      alpha = i

      if (checkLex(ps, i)) {
        ps
      } else {

        beta = -1
        while (i != size && notAlwaysLt(ps, i)) {
          if (ps.dom(scope(i)).head == ps.dom(scope(i + size)).head) {
            if (beta == -1) beta = i
          } else {
            beta = -1
          }
          i += 1
        }

        if (i == size) beta = Integer.MAX_VALUE
        else if (beta == -1) beta = i
        if (alpha >= beta) {
          Contradiction
        } else {
          gacLexLeq(ps, alpha)
        }
      }
    }
  }

  private def gacLexLeq(ps: ProblemState, i: Int): Outcome = {
    if (i >= beta) ps
    else {
      var mod = ps
      if (i == alpha && i + 1 == beta) {
        mod = acLt(mod, i)
        if (checkLex(mod, i)) {
          return mod
        }
      }
      if (i == alpha && i + 1 < beta) {
        mod = acLeq(mod, i)
        if (checkLex(mod, i)) {
          return mod
        }
        if (groundEq(mod, i)) {
          updateAlpha(mod, i + 1) match {
            case Contradiction   => return Contradiction
            case s: ProblemState => mod = s
          }
        }
      }
      if (alpha < i && i < beta) {
        if ((i == beta - 1 && mod.dom(scope(i)).head == mod.dom(scope(i + size)).last) || alwaysLt(mod, i)) {
          updateBeta(mod, i + 1) match {
            case Contradiction   => return Contradiction
            case s: ProblemState => mod = s
          }
        }
      }
      mod
    }
  }

  private def acLt(ps: ProblemState, i: Int): ProblemState = {
    val x = scope(i)
    val y = scope(i + size)
    ps.removeTo(y, ps.dom(x).head)
      .removeFrom(x, ps.dom(y).last)
      .asInstanceOf[ProblemState]
  }

  private def acLeq(ps: ProblemState, i: Int): ProblemState = {
    val x = scope(i)
    val y = scope(i + size)
    ps.removeUntil(y, ps.dom(x).head)
      .removeAfter(x, ps.dom(y).last)
      .asInstanceOf[ProblemState]

  }

  private def updateAlpha(ps: ProblemState, i: Int): Outcome = {
    if (i == beta) {
      Contradiction
    } else if (i == size) {
      ps
    } else if (!groundEq(ps, i)) {
      alpha = i
      gacLexLeq(ps, i)
    } else {
      updateAlpha(ps, i + 1)
    }

  }

  private def updateBeta(ps: ProblemState, i: Int): Outcome = {
    if (i + 1 == alpha) {
      Contradiction
    } else if (notAlwaysLt(ps, i)) {
      beta = i + 1
      gacLexLeq(ps, i)
    } else if (ps.dom(scope(i)).head == ps.dom(scope(i + size)).last) {
      updateBeta(ps, i - 1)
    } else {
      ps
    }
  }

  def advise(ps: ProblemState, p: Int) = size
  def simpleEvaluation = 2

}
