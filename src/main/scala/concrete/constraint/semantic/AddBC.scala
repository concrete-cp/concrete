package concrete.constraint.semantic;

import scala.Vector
import com.typesafe.scalalogging.LazyLogging
import concrete.Domain
import concrete.Revised
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.BC

final class AddBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC with LazyLogging {

  type State = Unit
  def initState = Unit

  def check(t: Array[Int]) = t(0) == t(1) + t(2)

  def shave(domains: IndexedSeq[Domain], s: State) = {
    val result = domains(0).span
    val v0 = domains(1).span
    val v1 = domains(2).span
    val bounds = v0 + v1 - result

    Revised(Vector(
      domains(0) & (bounds.lb + result.ub, bounds.ub + result.lb),
      domains(1) & (v0.ub - bounds.ub, v0.lb - bounds.lb),
      domains(2) & (v1.ub - bounds.ub, v1.lb - bounds.lb)))
  }

  override def toString(domains: IndexedSeq[Domain], s: State) =
    s"$result ${domains(0)} = $v0 ${domains(1)} + $v1 ${domains(2)}"

  def advise(domains: IndexedSeq[Domain], pos: Int) = 4

  def simpleEvaluation = 2
}
