package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.IntVariable
import cspom.variable.CSPOMExpression

/**
 * Transforms x = sub(y, z), [t =] ge(x, k) into [t =] diffGe(y, z, k)
 */
object DiffGe extends ConstraintCompiler {
  type A = CSPOMConstraint[Boolean]

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM) = constraint match {
    case CSPOMConstraint(result: IntVariable, 'sub, args, _) if (result.hasParam("var_is_introduced")) =>
      val constraints = problem.constraints(result)
      if (constraints.size == 2) {
        constraints.collectFirst {
          case c @ CSPOMConstraint(_: CSPOMExpression[Boolean], 'ge, Seq(`result`, _), _) => c.asInstanceOf[CSPOMConstraint[Boolean]]
        }
      } else {
        None
      }

    case _ => None

  }

  def compile(subConstraint: CSPOMConstraint[_], problem: CSPOM, geConstraint: CSPOMConstraint[Boolean]) = {

    val newC = CSPOMConstraint(geConstraint.result, 'diffGe, subConstraint.arguments :+ geConstraint.arguments(1))

    replaceCtr(Seq(subConstraint, geConstraint), newC, problem)

  }
  def selfPropagation = false
}
