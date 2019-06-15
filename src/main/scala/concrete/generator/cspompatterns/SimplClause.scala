package concrete.generator.cspompatterns

import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.variable.{BoolExpression, CSPOMConstant, CSPOMSeq}
import cspom.{CSPOM, CSPOMConstraint, UNSATException}
import CSPOM.seq2CSPOMSeq
import ConstraintCompiler._

/**
  * Removes constants from clauses
  */
object SimplClause extends ConstraintCompilerNoData {
  def functions = Functions("clause")

  def matchBool(fc: CSPOMConstraint[_], problem: CSPOM): Boolean = fc match {
    case CSPOMConstraint(_, _, Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]), _) =>
      (positive.iterator ++ negative).exists(_.isInstanceOf[CSPOMConstant[_]])

    case _ => false

  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]) = fc.arguments

    if (positive.exists(_.isTrue) || negative.exists(_.isFalse)) {
      val r = BoolExpression.coerce(fc.result)
      replaceCtr(fc, Seq(), problem) ++
        replace(fc.result, reduceDomain(r, d = true), problem)
    } else if (positive.forall(_.isFalse) && negative.forall(_.isTrue)) {
      val r = BoolExpression.coerce(fc.result)
      replaceCtr(fc, Seq(), problem) ++
        replace(fc.result, reduceDomain(r, d = false), problem)
    } else {
      val newP = positive.filterNot(_.isFalse)
      val newN = negative.filterNot(_.isTrue)
      if (newP.isEmpty && newN.isEmpty) throw new UNSATException("Empty clause created during compilation")
      replaceCtr(fc, CSPOMConstraint(fc.result, "clause", Seq(newP, newN), fc.params), problem)
    }

  }

}
