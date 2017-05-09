package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq

/**
 * Transforms x = !a, x \/ c \/ ... into !a \/ b \/ c \/ ...
 */
object MergeNotDisj extends ConstraintCompiler {
  type A = Seq[CSPOMConstraint[_]]

  override def mtch(fc: CSPOMConstraint[_], problem: CSPOM): Option[A] =
    PartialFunction.condOpt(fc) {
      case CSPOMConstraint(v: CSPOMExpression[_], 'not, _, _) =>
        problem.constraints(v).toSeq.collect {
          case c: CSPOMConstraint[_] if c.function == 'clause => c
        }
    }


  def compile(fc: CSPOMConstraint[_], problem: CSPOM, clauses: A) = {
    val Seq(a: CSPOMExpression[_]) = fc.arguments

    val newConstraints = for (clause <- clauses) yield {
      val Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]) = clause.arguments
      new CSPOMConstraint(clause.result, 'clause, Seq(positive, a +: negative))
    }

    replaceCtr(fc +: clauses, newConstraints, problem)
  }

  def selfPropagation = true
}
