package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.Variable
import concrete.ReviseOutcome
import concrete.constraint.BC
import concrete.Revised
import concrete.Domain
import concrete.constraint.StatelessBC

final class AbsBC(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0))
  with StatelessBC {

  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def check(t: Array[Int]) = t(0) == math.abs(t(1))

  def shave(domains: IndexedSeq[Domain]): ReviseOutcome[Unit] = {
    val result = domains(0)
    val v0 = domains(1)

    val nr = result & v0.span.abs

    val ri = result.span

    val nv0 = v0 & (ri span -ri)

    val nd = Vector(nr, nv0)

    Revised(nd, isFree(nd))
  }

  override def toString(domains: IndexedSeq[Domain]) = domains(0) + " = |" + domains(1) + "|";

  def advise(domains: IndexedSeq[Domain], p: Int) = 6

  def simpleEvaluation = 1
}
