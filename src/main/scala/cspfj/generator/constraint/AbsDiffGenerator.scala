package cspfj.generator.constraint;

import com.google.common.primitives.Ints
import cspfj.constraint.semantic.AbsDiff
import cspfj.exception.FailedGenerationException
import cspfj.problem.BitVectorDomain
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspom.constraint.CSPOMConstraint
import cspom.variable.CSPOMDomain
import scala.collection.immutable.TreeSet

final class AbsDiffGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generate(constraint: CSPOMConstraint) = {
    require(constraint.description == "absdiff")

    val Seq(result, v0, v1) = constraint.scope.map(getSolverVariable)

    if (Seq(result, v0, v1).count(_.getDomain == null) > 1) {
      false
    } else {
      if (result.getDomain == null) {

        val values = AbstractGenerator.makeDomain(
          AbstractGenerator.cartesian(v0, v1, (i, j) => math.abs(i - j)))

        result.setDomain(new BitVectorDomain(values));

      } else if (v0.getDomain == null) {

        v0.setDomain(new BitVectorDomain(generateValues(result, v1)));

      } else if (v1.getDomain == null) {

        v1.setDomain(new BitVectorDomain(generateValues(result, v0)));

      }
      addConstraint(new AbsDiff(result, v0, v1));
      true;
    }

  }

  private def generateValues(result: Variable, variable: Variable) = {
    AbstractGenerator.makeDomain(
      AbstractGenerator.cartesian(result, variable, (i, j) => Seq(i + j, i - j)).flatten)
  }
}