package concrete.generator.cspompatterns

import cspom.CSPOM.seq2CSPOMSeq
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData}
import cspom.variable.{BoolExpression, CSPOMConstant, CSPOMSeq}
import cspom.{CSPOM, CSPOMConstraint, UNSATException}
import ConstraintCompiler._

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
      val r = BoolExpression.coerce(fc.result)
      replaceCtr(fc, Seq(), problem) ++
        replace(fc.result, reduceDomain(r, true), problem)
    } else if (positive.forall(_.isFalse) && negative.forall(_.isTrue)) {
      val r = BoolExpression.coerce(fc.result)
      replaceCtr(fc, Seq(), problem) ++
        replace(fc.result, reduceDomain(r, false), problem)
    } else {
      val newP = positive.filterNot(_.isFalse)
      val newN = negative.filterNot(_.isTrue)
      if (newP.isEmpty && newN.isEmpty) throw new UNSATException("Empty clause created during compilation")
      replaceCtr(fc, CSPOMConstraint(fc.result, 'clause, Seq(newP, newN), fc.params), problem)
    }

  }

  def selfPropagation = false

}
