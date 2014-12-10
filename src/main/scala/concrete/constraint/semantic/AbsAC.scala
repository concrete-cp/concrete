package concrete.constraint.semantic;

import concrete.Domain
import concrete.ReviseOutcome
import concrete.Revised
import concrete.Variable
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint

final class AbsAC(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0))
  with BCCompanion {
  type State = Unit
  def initState = Unit
  def skipIntervals = true

  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def check(t: Array[Int]) = t(0) == math.abs(t(1))

  def revise(domains: IndexedSeq[Domain], s:State): ReviseOutcome[Unit] = {
    val result = domains(0)
    val x = domains(1)

    val nr = result.filter(v => x.present(v) || x.present(-v))
    val nx = x.filter(v => result.present(math.abs(v)))

    val nd = Vector(nr, nx)

    Revised(nd, isFree(nd))
  }

  override def toString(domains: IndexedSeq[Domain], s: State) = domains(0) + " = |" + domains(1) + "|";

  def advise(domains: IndexedSeq[Domain], p: Int) = if (skip(domains)) -1 else domains(0).size * 3 / 2 + domains(1).size

  def simpleEvaluation = 1
}
