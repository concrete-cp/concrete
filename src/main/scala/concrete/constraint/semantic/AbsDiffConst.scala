package concrete.constraint.semantic;

import scala.Vector

import concrete.Contradiction
import concrete.Domain
import concrete.ReviseOutcome
import concrete.Revised
import concrete.Variable
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.BC

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

  override def toString(domains: IndexedSeq[Domain], s: State) = domains(0) + " =AC= |" + domains(1) + " - " + domains(2) + "|";

  def getEvaluation(domains: IndexedSeq[Domain]) = if (skip(domains)) -1 else domains(0).size + domains(1).size

  def simpleEvaluation = 2
}

final class AbsDiffConstBC(val result: Int, val v0: Variable, val v1: Variable)
  extends Constraint(Array(v0, v1)) with BC {

  type State = Unit
  def initState = Unit

  def check(t: Array[Int]) = result == math.abs(t(0) - t(1))

  def shave(domains: IndexedSeq[Domain], s: State): ReviseOutcome[Unit] = {
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

  override def toString(domains: IndexedSeq[Domain], s: State) =
    s"$result ${domains(0)} =BC= |$v0 ${domains(1)} - $v1 ${domains(2)}|";

  def advise(domains: IndexedSeq[Domain], p: Int) = 5

  def simpleEvaluation = 1
}
