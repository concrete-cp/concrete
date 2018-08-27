package concrete.generator.cspompatterns

import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, Delta}
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompiler._

object MinMaxSimplify extends ConstraintCompiler {

  type A = Seq[CSPOMExpression[_]]

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[MinMaxSimplify.A] = {
    if (c.function == 'min || c.function == 'max) {
      val distinct = c.arguments.distinct
      if (distinct.size < c.arguments.size) {
        Some(distinct)
      } else {
        None
      }
    } else {
      None
    }
  }

  def compile(constraint: CSPOMConstraint[_], p: CSPOM, data: A): Delta = {
    val nc = CSPOMConstraint(constraint.result)(constraint.function)(data: _*)
    replaceCtr(constraint, nc, p)
  }
}