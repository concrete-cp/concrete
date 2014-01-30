package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta

/**
 * Transforms x = sub(y, z), [t =] ge(x, k) into [t =] diffGe(y, z, k)
 */
object DiffGe extends ConstraintCompiler {
  type A = CSPOMConstraint

  override def mtch(constraint: CSPOMConstraint, problem: CSPOM) = constraint match {
    case CSPOMConstraint(result: CSPOMVariable, 'sub, args, _) if (result.params("var_is_introduced")) =>
      val constraints = problem.constraints(result)
      if (constraints.size == 2) {
        constraints.collectFirst {
          case c @ CSPOMConstraint(_, 'ge, Seq(`result`, _), _) => c
        }
      } else {
        None
      }

    case _ => None

  }

  def compile(subConstraint: CSPOMConstraint, problem: CSPOM, geConstraint: CSPOMConstraint) = {

    val newC = new CSPOMConstraint(geConstraint.result, 'diffGe, subConstraint.arguments :+ geConstraint.arguments(1))

    replaceCtr(Seq(subConstraint, geConstraint), newC, problem)

  }

}
