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
import cspom.variable.BoolExpression
import cspom.variable.CSPOMFalse

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

  type A = BoolExpression

  override def constraintMatcher = {
    case CSPOMConstraint(res: BoolExpression, 'or, args, params) if (res != CSPOMTrue) =>
      res
  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, res: BoolExpression) = {
    val revsign = fc.params.get("revsign") match {
      case Some(r: Seq[Boolean]) => r
      case None => Seq.fill(fc.arguments.size)(false)
    }

    val reverses = true +: revsign

    val newCtr =
      new CSPOMConstraint(CSPOMTrue, 'or, res +: fc.arguments, fc.params + ("revsign" -> reverses)) +:
        (fc.arguments zip revsign).map {
          case (v, s) =>
            new CSPOMConstraint(CSPOMTrue, 'or, Seq(res, v), Map("revsign" -> Seq(false, !s)))
        }

    replaceCtr(fc, newCtr, problem)

  }

  def selfPropagation = false
}
