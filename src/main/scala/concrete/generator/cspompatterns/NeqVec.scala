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
  type A = (CSPOMConstraint, Set[CSPOMExpression], Set[CSPOMConstraint])

  /* NE constraints is a special case of nevec */
  private def isNevec(c: CSPOMConstraint) = c.function == 'ne || c.function == 'nevec

  override def mtch(c: CSPOMConstraint, problem: CSPOM) = {

    val result = c match {

      case CSPOMConstraint(result: CSPOMVariable, 'ne | 'nevec, _, _) => Some((problem.constraints(result) - c).toSeq)
      case _ => None
    }

    result collect {
      case Seq(orConstraint @ CSPOMConstraint(CSPOMTrue, 'or, _, _)) =>
        val orVariables = orConstraint.fullScope.toSet
        val neConstraints = orVariables.flatMap(problem.constraints) - orConstraint
        (orConstraint, orVariables, neConstraints)
    } filter {
      _._3.forall(isNevec)
    }

  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, data: (CSPOMConstraint, Set[CSPOMExpression], Set[CSPOMConstraint])) = {
    val (orConstraint, orVariables, neConstraints) = data

    val (x, y) = neConstraints.map(_.arguments).foldLeft((Seq[CSPOMExpression](), Seq[CSPOMExpression]())) {
      case ((ax, ay), Seq(cx: CSPOMSeq[CSPOMExpression], cy: CSPOMSeq[CSPOMExpression])) => (ax ++ cx.values, ay ++ cy.values)
      case ((ax, ay), Seq(cx: CSPOMVariable, cy: CSPOMVariable)) => (ax :+ cx, ay :+ cy)
      case _ => throw new IllegalArgumentException(s"$neConstraints contains malformed ne/nevec constraint")
    }

    val newC = problem.ctr(new CSPOMConstraint('nevec, new CSPOMSeq(x), new CSPOMSeq(y)))

    replaceCtr(orConstraint +: neConstraints.toSeq, newC, problem)

  }
  
  def selfPropagation = true

}
