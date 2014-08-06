package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspom.variable.CSPOMConstant
import Generator._
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.Cumulative
import concrete.IntDomain

final object CumulativeGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val Seq(
      Sequence(starts, _),
      Sequence(durations, _),
      Sequence(resources, _),
      limit) = constraint.arguments.map(cspom2concrete)

    val cLimit = limit.asVariable

    val cStarts = starts.map(_.asVariable).toArray
    val cDurations = durations.map(_.asVariable).toArray
    val cResources = resources.map(_.asVariable).toArray

    Seq(new Cumulative(cStarts, cDurations, cResources, cLimit))

  }

}
