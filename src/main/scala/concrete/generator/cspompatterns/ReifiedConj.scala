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

  type A = CSPOMExpression[_]

  override def constraintMatcher = {
    case CSPOMConstraint(res: SimpleExpression[_], 'and, args, params) =>
      res
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, res: CSPOMExpression[_]) = {
    val c1 =
      CSPOMConstraint('clause, Seq(CSPOMSeq(res), CSPOMSeq(fc.arguments: _*)), fc.params)

    val c2 = fc.arguments.map {
      case v =>
        CSPOMConstraint('clause, Seq(CSPOMSeq(v), CSPOMSeq(res)))
    }

    replaceCtr(fc, c1 +: c2, problem)

  }

  def selfPropagation = false

}
