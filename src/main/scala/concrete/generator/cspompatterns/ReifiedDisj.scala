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
 * Reified disjunction is converted to CNF :
 *
 * a = b v c v d...
 *
 * <=>
 *
 * (-a v b v c v d...) ^ (a v -b) ^ (a v -c) ^ (a v -d) ^ ...
 */
object ReifiedDisj extends ConstraintCompiler {

  type A = SimpleExpression[_]

  override def constraintMatcher = {
    case CSPOMConstraint(res: SimpleExpression[_], 'or, args, params) if (!res.isTrue) =>
      res
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, res: SimpleExpression[_]) = {
    val revsign = fc.params.get("revsign") match {
      case Some(r: Seq[_]) => r.map(_.asInstanceOf[Boolean])
      case None => Seq.fill(fc.arguments.size)(false)
      case o => throw new MatchError(o)
    }

    val reverses = true +: revsign

    val newCtr =
      CSPOMConstraint('or, res +: fc.arguments, fc.params + ("revsign" -> reverses)) +:
        (fc.arguments zip revsign).map {
          case (v, s) =>
            CSPOMConstraint('or, Seq(res, v), Map("revsign" -> Seq(false, !s)))
        }

    replaceCtr(fc, newCtr, problem)

  }

  def selfPropagation = false
}
