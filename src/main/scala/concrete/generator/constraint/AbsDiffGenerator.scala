package concrete.generator.constraint;

import concrete.constraint.semantic.AbsDiff
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import concrete.UndefinedDomain
import Generator._
import concrete.constraint.semantic.AbsDiffConst
import concrete.constraint.Constraint

final object AbsDiffGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc)(implicit problem: Problem) = {
    val result = r.asInstanceOf[C21D]

    val Seq(v0, v1) = constraint.arguments.map(cspom2concreteVar)

    val g = undefinedVar(result, v0, v1) match {
      case Seq() => true;
      case Seq(r) if result.is(r) =>
        val values = domainFrom(v0, v1, { case (i, j) => math.abs(i - j) })
        restrictDomain(r, values)
        true
      case Seq(`v0`) =>
        restrictDomain(v0, generateValues(result, v1))
        true
      case Seq(`v1`) =>
        restrictDomain(v1, generateValues(result, v0))
        true
      case _ =>
        false
    }

    if (g) {
      Some(Seq(result match {
        case Var(r) => new AbsDiff(r, v0, v1)
        case Const(c) => new AbsDiffConst(c, v0, v1)
      }))
    } else {
      None
    }

  }

  def f2(s: Seq[Int]) = {
    val Seq(i, j) = s
    Seq(i + j, i - j)
  }

  private def generateValues(result: C21D, variable: Variable) = {
    domainFromFlatSeq(Seq(result.values, variable.dom.values.toSeq), f2(_))

  }
}
