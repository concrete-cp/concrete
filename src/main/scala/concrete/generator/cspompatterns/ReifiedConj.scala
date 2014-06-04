package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMExpression
import cspom.variable.SimpleExpression

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

  type A = CSPOMExpression[_]

  override def constraintMatcher = {
    case CSPOMConstraint(res: SimpleExpression[_], 'and, args, params) if (!res.isTrue) =>
      res
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, res: CSPOMExpression[_]) = {
    require(!fc.params.contains("revsign")) // Conjunctions should not be parameterized -- yet

    val reverses = Seq(false).padTo(fc.arguments.size + 1, true)

    val c1 =
      CSPOMConstraint('or, res +: fc.arguments, fc.params + ("revsign" -> reverses))

    val c2 = fc.arguments.map {
      case v =>
        CSPOMConstraint('or, Seq(res, v), Map("revsign" -> Seq(true, false)))
    }

    replaceCtr(fc, c1 +: c2, problem)

  }

  def selfPropagation = false

}
