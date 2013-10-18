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

  def mtch(constraint: CSPOMConstraint, problem: CSPOM) = {
    if (constraint.function == 'sub) {
      constraint.result match {

        case v: CSPOMVariable if (v.params("var_is_introduced")) =>
          val constraints = problem.constraints(v)
          if (constraints.size == 2) {
            constraints.find { c =>
              c.function == 'ge && {
                c.arguments.size == 2 && c.arguments(0) == constraint.result
              }
            }
          } else {
            None
          }

        case _ => None
      }
    } else {
      None
    }
  }

  def compile(subConstraint: CSPOMConstraint, problem: CSPOM, geConstraint: CSPOMConstraint) = {

    problem.removeConstraint(subConstraint);
    problem.removeConstraint(geConstraint);
    problem.removeVariable(subConstraint.result.asInstanceOf[CSPOMVariable])

    val newC = problem.ctr(new CSPOMConstraint(geConstraint.result, 'diffGe, subConstraint.arguments :+ geConstraint.arguments(1)))

    Delta(Seq(subConstraint, geConstraint), newC.scope)
  }

}
