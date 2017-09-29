package concrete.generator.cspompatterns

import cspom.CSPOM.seq2CSPOMSeq
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.ConstraintCompiler
import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq, CSPOMVariable}

/**
  * Transforms x = (a, b, ...) ne (c, d, ...), y = (e, f, ...) ne (g, h, ...), ..., x \/ y \/ ...
  * into (a, b, e, f, ...) ne (c, d, g, h, ...)
  */
object NeqVec extends ConstraintCompiler {
  type A = (CSPOMConstraint[_], Set[CSPOMExpression[_]], Set[CSPOMConstraint[_]])

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = {

    PartialFunction.condOpt(c) {
      case CSPOMConstraint(result, 'ne | 'nevec, _, _) if result.searchSpace > 1 => (problem.constraints(result) - c).toSeq
    }
      .collect {
        case Seq(orConstraint@CSPOMConstraint(CSPOMConstant(true), 'clause, Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]), _)) if negative.isEmpty =>
          val orVariables: Set[CSPOMExpression[_]] = positive.toSet
          val neConstraints = orVariables.flatMap(problem.constraints) - orConstraint
          (orConstraint, orVariables, neConstraints)
      } filter {
      _._3.forall(isNevec)
    }

  }

  /* NE constraints is a special case of nevec */
  private def isNevec(c: CSPOMConstraint[_]) = c.function == 'ne || c.function == 'nevec

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val (orConstraint, orVariables, neConstraints) = data

    val (x, y) = neConstraints.map(_.arguments).foldLeft((Seq[CSPOMExpression[Any]](), Seq[CSPOMExpression[Any]]())) {
      case ((ax, ay), Seq(cx: CSPOMSeq[_], cy: CSPOMSeq[_])) => (ax ++ cx.values, ay ++ cy.values)
      case ((ax, ay), Seq(cx: CSPOMVariable[_], cy: CSPOMVariable[_])) => (ax :+ cx, ay :+ cy)
      case _ => throw new IllegalArgumentException(s"$neConstraints contains malformed ne/nevec constraint")
    }

    val newC = problem.ctr(CSPOMConstraint('nevec)(x, y))

    replaceCtr(orConstraint +: neConstraints.toSeq, newC, problem)

  }

  def selfPropagation = true

}
