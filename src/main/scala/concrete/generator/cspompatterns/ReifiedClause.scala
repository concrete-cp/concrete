package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta
import cspom.variable.BoolExpression
import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression

/**
 * Reified disjunction is converted to CNF :
 *
 * a = b v c v d...
 *
 * <=>
 *
 * (-a v b v c v d...) ^ (a v -b) ^ (a v -c) ^ (a v -d) ^ ...
 */
object ReifiedClause extends ConstraintCompiler {

  type A = SimpleExpression[Boolean]

  override def constraintMatcher = {
    case CSPOMConstraint(BoolExpression(res), 'clause, args, params) if (!res.isTrue) =>
      res
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, res: SimpleExpression[Boolean]) = {
    val Seq(BoolExpression.simpleSeq(positive), BoolExpression.simpleSeq(negative)) = fc.arguments

    val c1 = CSPOMDriver.clause(positive: _*)(res +: negative: _*)
    val c2 = positive.map {
      case v => CSPOMDriver.clause(res)(v)
    }
    val c3 = negative.map {
      case v => CSPOMDriver.clause(res, v)()
    }

    replaceCtr(fc, c1 +: (c2 ++ c3), problem)

  }

  def selfPropagation = false
}
