package concrete.constraint.semantic;

import concrete.Domain
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.util.Interval
import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.BC
import concrete.constraint.StatelessBC
import concrete.Revised

final class AddBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with StatelessBC with LazyLogging {

  def check(t: Array[Int]) = t(0) == t(1) + t(2)

  def shave(domains: IndexedSeq[Domain]) = {
    val result = domains(0).span
    val v0 = domains(1).span
    val v1 = domains(2).span
    val bounds = v0 + v1 - result

    Revised(Vector(
      domains(0) & (bounds.lb + result.ub, bounds.ub + result.lb),
      domains(1) & (v0.ub - bounds.ub, v0.lb - bounds.lb),
      domains(2) & (v1.ub - bounds.ub, v1.lb - bounds.lb)))
  }

  override def toString(domains: IndexedSeq[Domain]) = domains(0) + " = " + domains(1) + " + " + domains(2)

  def advise(domains: IndexedSeq[Domain], pos: Int) = 4

  def simpleEvaluation = 2
}
