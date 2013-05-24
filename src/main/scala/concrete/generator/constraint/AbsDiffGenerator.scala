package concrete.generator.constraint;

import concrete.constraint.semantic.AbsDiff
import concrete.{ Variable, Problem, IntDomain }
import cspom.constraint.CSPOMConstraint
import concrete.UndefinedDomain

final class AbsDiffGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generate(constraint: CSPOMConstraint) = {
    require(constraint.description == "absdiff")

    val Seq(result, v0, v1) = constraint.scope.map(cspom2concrete)

    val g = Seq(result, v0, v1).filter(_.dom.undefined) match {
      case Seq() => true;
      case Seq(`result`) =>
        val values = AbstractGenerator.domainFrom(v0, v1, (i, j) => math.abs(i - j))
        result.dom = IntDomain(values: _*)
        true
      case Seq(`v0`) =>
        v0.dom = IntDomain(generateValues(result, v1): _*)
        true
      case Seq(`v1`) =>
        v1.dom = IntDomain(generateValues(result, v0): _*)
        true
      case _ =>
        false
    }

    if (g) {
      addConstraint(new AbsDiff(result, v0, v1));
    }
    g

  }

  private def generateValues(result: Variable, variable: Variable) = {
    AbstractGenerator.makeDomain(
      AbstractGenerator.cartesian(result, variable, (i, j) => Seq(i + j, i - j)).flatten)
  }
}
