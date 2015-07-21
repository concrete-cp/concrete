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
import cspom.variable.CSPOMSeq
import CSPOM._

/**
 * Removes constants from clauses
 */
object SimplClause extends ConstraintCompilerNoData {

  def matchBool(fc: CSPOMConstraint[_], problem: CSPOM) = fc match {
    case CSPOMConstraint(_, 'clause, Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]), _) =>
      (positive.iterator ++ negative).exists(_.isInstanceOf[CSPOMConstant[_]])

    case _ => false

  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM) = {
    val Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]) = fc.arguments

    if (positive.exists(_.isTrue) || negative.exists(_.isFalse)) {
      replaceCtr(fc, Seq(), problem)
    } else {
      val newP = positive.filterNot(_.isFalse)
      val newN = negative.filterNot(_.isTrue)
      replaceCtr(fc, CSPOMConstraint(fc.result, 'clause, Seq(newP, newN), fc.params), problem)
    }

  }

  def selfPropagation = false

}
