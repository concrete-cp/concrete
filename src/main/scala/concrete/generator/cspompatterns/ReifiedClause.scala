package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.BoolExpression
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

  type A = (SimpleExpression[Boolean], Seq[SimpleExpression[Boolean]], Seq[SimpleExpression[Boolean]])

  override def constraintMatcher = {
    case CSPOMConstraint(BoolExpression(res), 'clause, Seq(BoolExpression.simpleSeq(p), BoolExpression.simpleSeq(n)), params) if (!res.isTrue) =>
      (res, p, n)
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val (res, positive, negative) = data

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
