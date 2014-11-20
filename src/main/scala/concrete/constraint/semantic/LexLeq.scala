package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.Variable
import concrete.constraint.TupleEnumerator
import concrete.UNSATException
import concrete.UNSATObject
import concrete.constraint.Stateless
import concrete.Revised
import concrete.ReviseOutcome
import concrete.Domain
import concrete.Contradiction

final class LexLeq(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) with Stateless {
  val size = x.length
  require(size == y.length)

  private def groundEq(domains: IndexedSeq[Domain], i: Int) = {
    domains(i).size == 1 && domains(i + size).size == 1 && domains(i).head == domains(i + size).head
  }

  def check(t: Array[Int]) = ???

  private def notAlwaysLt(domains: IndexedSeq[Domain], i: Int) = domains(i).head <= domains(i + size).last

  private def alwaysLeq(domains: IndexedSeq[Domain], i: Int) = domains(i).last <= domains(i + size).head

  private def alwaysLt(domains: IndexedSeq[Domain], i: Int) = domains(i).last < domains(i + size).head

  private def checkLex(domains: IndexedSeq[Domain], i: Int) = {
    if (i == size - 1) alwaysLeq(domains, i)
    else alwaysLt(domains, i)
  }

  var alpha = -1
  var beta = -1

  def revise(domains: IndexedSeq[Domain]): ReviseOutcome[Unit] = {
    var i = 0
    while (i < size && groundEq(domains, i)) i += 1

    if (i == size) {
      Revised(domains)
    } else {
      alpha = i

      if (checkLex(domains, i)) {
        Revised(domains)
      } else {

        beta = -1
        while (i != size && notAlwaysLt(domains, i)) {
          if (domains(i).head == domains(i + size).head) {
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
          gacLexLeq(domains, alpha)
        }
      }
    }
  }

  private def gacLexLeq(domains: IndexedSeq[Domain], i: Int): ReviseOutcome[Unit] = {
    if (i >= beta) Revised(domains)
    else {
      var d = domains
      if (i == alpha && i + 1 == beta) {
        d = acLt(d, i)
        if (checkLex(d, i)) {
          return Revised(d)
        }
      }
      if (i == alpha && i + 1 < beta) {
        d = acLeq(d, i)
        if (checkLex(d, i)) {
          return Revised(d)
        }
        if (groundEq(d, i)) {
          updateAlpha(d, i + 1) match {
            case Contradiction      => return Contradiction
            case Revised(mod, _, _) => d = mod
          }
        }
      }
      if (alpha < i && i < beta) {
        if ((i == beta - 1 && d(i).head == d(i + size).last) || alwaysLt(d, i)) {
          updateBeta(d, i + 1) match {
            case Contradiction      => return Contradiction
            case Revised(mod, _, _) => d = mod
          }
        }
      }
      Revised(d)
    }
  }

  private def acLt(domains: IndexedSeq[Domain], i: Int): IndexedSeq[Domain] = {
    val d1 = domains.updated(i + size, domains(i + size).removeTo(domains(i).head))
    d1.updated(i, d1(i).removeFrom(d1(i + size).last))
  }

  private def acLeq(domains: IndexedSeq[Domain], i: Int): IndexedSeq[Domain] = {
    val d1 = domains.updated(i + size, domains(i + size).removeUntil(domains(i).head))
    d1.updated(i, d1(i).removeAfter(d1(i + size).last))
  }

  private def updateAlpha(domains: IndexedSeq[Domain], i: Int): ReviseOutcome[Unit] = {
    if (i == beta) {
      Contradiction
    } else if (i == size) {
      Revised(domains)
    } else if (!groundEq(domains, i)) {
      alpha = i
      gacLexLeq(domains, i)
    } else {
      updateAlpha(domains, i + 1)
    }

  }

  private def updateBeta(domains: IndexedSeq[Domain], i: Int): ReviseOutcome[Unit] = {
    if (i + 1 == alpha) {
      Contradiction
    } else if (domains(i).head < domains(i + size).last) {
      beta = i + 1
      if (notAlwaysLt(domains, i)) { gacLexLeq(domains, i) } else Revised(domains)
    } else if (domains(i).head == domains(i + size).last) updateBeta(domains, i - 1) else Revised(domains)
  }

  def advise(domains: IndexedSeq[Domain], p: Int) = size
  def simpleEvaluation = 2

}
