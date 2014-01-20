package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompiler
import cspom.compiler.Delta
import cspom.compiler.Delta
import cspom.variable.BoolExpression
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMFalse
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMVariable

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object RemoveUselessEq extends ConstraintCompiler {

  type A = BoolExpression

  override def constraintMatcher = {
    case CSPOMConstraint(res: BoolExpression, 'eq, args, _) if allEqual(args) =>
      res
    case CSPOMConstraint(res: BoolExpression, 'eq, args, _) if args.forall(_.isInstanceOf[CSPOMConstant]) =>
      res
  }

  def compile(c: CSPOMConstraint, problem: CSPOM, res: A): Delta = {
    problem.removeConstraint(c)
    val delta = Delta().removed(c)

    val result = if (allEqual(c.arguments)) {
      CSPOMTrue
    } else {
      CSPOMFalse
    }

    res match {
      case b: CSPOMVariable => delta ++ replaceVars(Seq(b), result, problem)
      case c: CSPOMConstant =>
        require(c == result); delta
      case _ => throw new IllegalArgumentException
    }

  }

  private def allEqual[A](s: Seq[A]) = s.forall(_ == s.head)

}
