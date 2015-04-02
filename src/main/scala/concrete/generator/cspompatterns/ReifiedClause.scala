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
 * Reified disjunction is converted to CNF :
 *
 * a = b v c v d...
 *
 * <=>
 *
 * (-a v b v c v d...) ^ (a v -b) ^ (a v -c) ^ (a v -d) ^ ...
 */
object ReifiedClause extends ConstraintCompiler {

  type A = SimpleExpression[_]

  override def constraintMatcher = {
    case CSPOMConstraint(res: SimpleExpression[_], 'clause, args, params) if (!res.isTrue) =>
      res
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, res: SimpleExpression[_]) = {
    val Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]) = fc.arguments

    val c1 = CSPOMConstraint('clause, Seq(positive, res +: negative))
    val c2 = positive.map { v =>
      CSPOMConstraint('clause, Seq(CSPOMSeq(res), CSPOMSeq(v)))
    }
    val c3 = negative.map { v =>
      CSPOMConstraint('clause, Seq(CSPOMSeq(res, v), CSPOMSeq()))
    }

    replaceCtr(fc, c1 +: (c2 ++ c3), problem)

  }

  def selfPropagation = false
}
