package cspfj.generator.constraint;

import cspfj.constraint.semantic.AbsDiff
import cspfj.problem.{ Variable, Problem, BitVectorDomain }
import cspom.constraint.CSPOMConstraint

final class AbsDiffGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generate(constraint: CSPOMConstraint) = {
    require(constraint.description == "absdiff")

    val Seq(result, v0, v1) = constraint.scope.map(cspom2cspfj)

    if (Seq(result, v0, v1).count(_.domain == null) > 1) {
      false
    } else {
      if (result.domain == null) {

        val values = AbstractGenerator.domainFrom(v0, v1, (i, j) => math.abs(i - j))

        result.domain = new BitVectorDomain(values: _*);

      } else if (v0.domain == null) {

        v0.domain = new BitVectorDomain(generateValues(result, v1): _*);

      } else if (v1.domain == null) {

        v1.domain = new BitVectorDomain(generateValues(result, v0): _*);

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