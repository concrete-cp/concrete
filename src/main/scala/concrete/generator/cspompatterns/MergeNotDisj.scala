package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq

/**
  * Transforms x = !a, x \/ c \/ ... into !a \/ b \/ c \/ ...
  */
object MergeNotDisj extends ConstraintCompiler {
  type A = Seq[CSPOMConstraint[_]]

  def functions = Functions("not")

  override def mtch(fc: CSPOMConstraint[_], problem: CSPOM): Option[A] = Some {
    val v = fc.result
    problem.constraints(v).toSeq.collect {
      case c: CSPOMConstraint[_] if c.function == "clause" => c
    }
  }


  def compile(fc: CSPOMConstraint[_], problem: CSPOM, clauses: A): Delta = {
    val Seq(a: CSPOMExpression[_]) = fc.arguments

    val newConstraints = for (clause <- clauses) yield {
      val Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]) = clause.arguments
      new CSPOMConstraint(clause.result, "clause", Seq(positive, a +: negative))
    }

    ConstraintCompiler.replaceCtr(fc +: clauses, newConstraints, problem)
  }

  def selfPropagation = true
}
