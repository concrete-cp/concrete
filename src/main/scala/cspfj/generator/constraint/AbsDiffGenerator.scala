package cspfj.generator.constraint;

import cspfj.constraint.semantic.AbsDiff
import cspfj.problem.{ Variable, Problem, IntDomain }
import cspom.constraint.CSPOMConstraint

final class AbsDiffGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generate(constraint: CSPOMConstraint) = {
    require(constraint.description == "absdiff")

    val Seq(result, v0, v1) = constraint.scope.map(cspom2cspfj)

    if (Seq(result, v0, v1).count(_.dom == null) > 1) {
      false
    } else {
      if (result.dom == null) {

        val values = AbstractGenerator.domainFrom(v0, v1, (i, j) => math.abs(i - j))

        result.dom = new IntDomain(values);

      } else if (v0.dom == null) {

        v0.dom = new IntDomain(generateValues(result, v1));

      } else if (v1.dom == null) {

        v1.dom = new IntDomain(generateValues(result, v0));

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