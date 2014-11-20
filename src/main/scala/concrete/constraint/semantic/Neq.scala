package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.Domain
import concrete.Variable
import concrete.ReviseOutcome
import concrete.Contradiction
import concrete.Revised
import concrete.constraint.Stateless

final class Neq(v0: Variable, v1: Variable) extends Constraint(Array(v0, v1)) with Stateless {

  def check(t: Array[Int]) = t(0) != t(1)

  def revise(domains: IndexedSeq[Domain]): ReviseOutcome[Unit] = {
    val d0 = revise(domains(0), domains(1))
    val d1 = revise(domains(1), domains(0))

    Revised(IndexedSeq(d0, d1), d0 disjoint d1)
  }

  private def revise(variable: Domain, otherVar: Domain): Domain = {
    if (variable.size == 1) otherVar.remove(variable.head) else otherVar
  }

  override def isConsistent(domains: IndexedSeq[Domain]) = {
    domains(0).size > 1 || domains(1).size > 1 || domains(0).head != domains(1).head
  }

  override def toString = v0 + " /= " + v1

  def advise(domains: IndexedSeq[Domain], p: Int) = if (domains(p).size > 1) -1 else 2

  val simpleEvaluation = 2
}
