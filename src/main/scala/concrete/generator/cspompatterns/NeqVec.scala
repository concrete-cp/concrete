package concrete.generator.cspompatterns

import scala.util.control.Breaks._

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.Delta
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMVariable

/**
 * Transforms x = (a, b, ...) ne (c, d, ...), y = (e, f, ...) ne (g, h, ...), ..., x \/ y \/ ...
 * into (a, b, e, f, ...) ne (c, d, g, h, ...)
 */
object NeqVec extends ConstraintCompiler {
  type A = (CSPOMConstraint, Set[CSPOMVariable], Set[CSPOMConstraint])

  /* NE constraints is a special case of nevec */
  private def isNevec(c: CSPOMConstraint) = c.function == "ne" || c.function == "nevec"

  def mtch(c: CSPOMConstraint, problem: CSPOM) = {
    if (isNevec(c)) {
      c.result match {
        case v: CSPOMVariable => problem.constraints(v).filter(_ != c) match {
          case Seq(orConstraint) if (orConstraint.function == "or" && orConstraint.result == CSPOMTrue) =>
            val orVariables = orConstraint.scope
            val neConstraints = orVariables.flatMap(problem.constraints) - orConstraint

            if (neConstraints.forall(isNevec)) {
              Some((orConstraint, orVariables, neConstraints))
            } else {
              None
            }
          case _ => None
        }
        case _ => None
      }
    } else {
      None
    }
  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, data: (CSPOMConstraint, Set[CSPOMVariable], Set[CSPOMConstraint])) = {
    val (orConstraint, orVariables, neConstraints) = data

    neConstraints.foreach(problem.removeConstraint)
    problem.removeConstraint(orConstraint)
    orVariables.foreach(problem.removeVariable)

    val (x, y) = neConstraints.map(_.arguments).foldLeft((Seq[CSPOMExpression](), Seq[CSPOMExpression]())) {
      case ((ax, ay), Seq(cx: CSPOMSeq, cy: CSPOMSeq)) => (ax ++ cx.values, ay ++ cy.values)
      case ((ax, ay), Seq(cx: CSPOMVariable, cy: CSPOMVariable)) => (ax :+ cx, ay :+ cy)
      case _ => throw new IllegalArgumentException(s"$neConstraints contains malformed ne/nevec constraint")
    }

    val newC = problem.addConstraint(new CSPOMConstraint("nevec", new CSPOMSeq(x), new CSPOMSeq(y)))

    new Delta(orConstraint :: neConstraints.toList, newC.scope)
  }

}
