package concrete.generator.constraint;

import concrete.constraint.semantic.AbsDiff
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import concrete.UndefinedDomain
import AbstractGenerator._

final class AbsDiffGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc) = {
    val Var(result) = r
    val Seq(v0, v1) = constraint.arguments.map(cspom2concrete).collect { case Var(v) => v }

    val g = Seq(result, v0, v1).filter(_.dom.undefined) match {
      case Seq() => true;
      case Seq(`result`) =>
        val values = domainFromVar(Seq(v0, v1), { case Seq(i, j) => math.abs(i - j) })
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
      addConstraint(new AbsDiff(result, v0, v1))
    }
    g

  }

  private def generateValues(result: Variable, variable: Variable) = {

    domainFromVarFlat(Seq(result, variable), { case Seq(i, j) => Seq(i + j, i - j) })

  }
}
