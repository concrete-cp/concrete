package concrete.generator.cspompatterns

import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompiler._

object MinMaxSimplify extends ConstraintCompiler {

  def functions = Functions('min, 'max)

  type A = Seq[CSPOMExpression[_]]

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[MinMaxSimplify.A] = {
    val distinct = c.arguments.distinct
    if (distinct.size < c.arguments.size) {
      Some(distinct)
    } else {
      None
    }
  }

  def compile(constraint: CSPOMConstraint[_], p: CSPOM, data: A): Delta = {
    val nc = CSPOMConstraint(constraint.result)(constraint.function)(data: _*)
    replaceCtr(constraint, nc, p)
  }
}