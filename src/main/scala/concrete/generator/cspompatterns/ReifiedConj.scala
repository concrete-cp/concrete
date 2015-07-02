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
import cspom.variable.CSPOMSeq
import cspom.variable.BoolExpression
import concrete.CSPOMDriver

/**
 * Conjunction is converted to CNF :
 *
 * a = b ^ c ^ d...
 *
 * <=>
 *
 * (a v -b v -c v -d...) ^ (-a v b) ^ (-a v c) ^ (-a v d) ^ ...
 */
object ReifiedConj extends ConstraintCompiler {

  type A = (SimpleExpression[Boolean], Seq[SimpleExpression[Boolean]])

  override def constraintMatcher = {
    case CSPOMConstraint(BoolExpression(res), 'and, BoolExpression.simpleSeq(args), _) =>
      (res, args)
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val (res, args) = data
    val c1 =
      CSPOMDriver.clause(res)(args: _*)

    val c2 = args.map(v =>
      CSPOMDriver.clause(v)(res))

    replaceCtr(fc, c1 +: c2, problem)

  }

  def selfPropagation = false

}
