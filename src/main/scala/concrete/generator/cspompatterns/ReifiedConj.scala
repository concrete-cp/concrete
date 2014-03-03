package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMFalse

/**
 * Reified conjunction is converted to CNF :
 *
 * a = b ^ c ^ d...
 *
 * <=>
 *
 * (a v -b v -c v -d...) ^ (-a v b) ^ (-a v c) ^ (-a v d) ^ ...
 */
object ReifiedConj extends ConstraintCompiler {

  type A = CSPOMExpression[Boolean]

  override def constraintMatcher = {
    case CSPOMConstraint(res: CSPOMExpression[Boolean], 'and, args, params) if (res != CSPOMTrue && !params.contains("revsign")) =>
      res
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, res: CSPOMExpression[Boolean]) = {
    val reverses = Seq(false).padTo(1 + fc.arguments.size, true)

    val c1 =
      CSPOMConstraint('or, res +: fc.arguments, fc.params + ("revsign" -> reverses))

    val c2 = fc.arguments.map {
      v =>
        CSPOMConstraint('or, Seq(res, v), Map("revsign" -> Seq(true, false)))
    }

    replaceCtr(fc, c1 +: c2, problem)

  }

  def selfPropagation = false

}
