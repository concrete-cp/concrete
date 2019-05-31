package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.compiler.{CompiledFunctions, ConstraintCompiler, Delta, Functions}
import cspom.variable.{CSPOMExpression, CSPOMSeq}
import cspom.{CSPOM, CSPOMConstraint}

object ClauseDuplicates extends ConstraintCompiler {

  type A = (Seq[CSPOMExpression[_]], Seq[CSPOMExpression[_]])

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM): Option[A] = constraint match {
    case CSPOMConstraint(_, 'clause, Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]), _) =>
      val allVars = positive.values ++ negative.values
      if (allVars.length != allVars.distinct.length) {
        Some((positive, negative))
      } else {
        None
      }

    case _ => None
  }

  override def compile(constraint: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {
    val (positive, negative) = data

    val noDupP1 = positive.toSet
    val noDupN1 = negative.toSet

    if (noDupP1.exists(noDupN1) || noDupN1.exists(noDupP1)) {
      ConstraintCompiler.removeCtr(constraint, problem)
    } else {
      ConstraintCompiler.replaceCtr(constraint, CSPOMDriver.clause(noDupP1.toSeq: _*)(noDupN1.toSeq: _*), problem)
    }
  }

  override def functions: CompiledFunctions = Functions('clause)
}
