package concrete.constraint.semantic;

import concrete.constraint.Residues
import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.UNSATException
import concrete.util.Interval
import concrete.constraint.BC
import concrete.UNSATObject
import concrete.constraint.BCCompanion
import concrete.constraint.Stateless
import concrete.constraint.StatelessBC
import concrete.Revised
import concrete.ReviseOutcome
import concrete.Contradiction

final class AbsDiffConstAC(val result: Int, val v0: Variable, val v1: Variable)
  extends Constraint(Array(v0, v1)) with BCCompanion with Residues {

  def skipIntervals = false

  def check(t: Array[Int]) = result == math.abs(t(0) - t(1))

  override def findSupport(domains: IndexedSeq[Domain], position: Int, value: Int) = {
    val other = domains(1 - position)

    Seq(value + result, value - result).find(
      v => other.present(v)).map { v =>

        val tuple = new Array[Int](2)
        tuple(position) = value
        tuple(1 - position) = v
        tuple
      }

  }

  override def toString = result + " =AC= |" + v0 + " - " + v1 + "|";

  def getEvaluation(domains: IndexedSeq[Domain]) = if (skip(domains)) -1 else domains(0).size + domains(1).size

  def simpleEvaluation = 2
}

final class AbsDiffConstBC(val result: Int, val v0: Variable, val v1: Variable)
  extends Constraint(Array(v0, v1)) with StatelessBC {

  def check(t: Array[Int]) = result == math.abs(t(0) - t(1))

  def shave(domains: IndexedSeq[Domain]): ReviseOutcome[Unit] = {
    val i0 = domains(0).span
    val i1 = domains(1).span

    val diff = i0 - i1

    if (!diff.abs.contains(result)) { Contradiction } else {

      val mod = if (diff.lb >= 0) {
        Vector(domains(0) & (i1 + result), domains(1) & (i0 - result))
      } else if (diff.ub <= 0) {
        Vector(domains(0) & (i1 - result), domains(1) & (i0 + result))
      } else {
        Vector(domains(0) & ((i1 + result) span (i1 - result)), domains(1) & ((i0 - result) span (i0 + result)))
        //        if (AbsDiffBC.unionInter(v0.dom, i0, i1 + result, i0, i1 - result)) {
        //          mod ::= 0
        //        }
        //        if (AbsDiffBC.unionInter(v1.dom, i1, i0 - result, i1, i0 + result)) {
        //          mod ::= 1
        //        }
      }

      Revised(mod)
    }
  }

  override def toString = result + " =BC= |" + v0 + " - " + v1 + "|";

  def advise(domains: IndexedSeq[Domain],p: Int) = 5

  def simpleEvaluation = 1
}
