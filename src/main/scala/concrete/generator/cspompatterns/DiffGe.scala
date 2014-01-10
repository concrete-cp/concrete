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

  val findSub: PartialFunction[(CSPOMConstraint, CSPOM), (CSPOMVariable, Set[CSPOMConstraint])] = {
    case (CSPOMConstraint(result: CSPOMVariable, 'sub, _, _), problem) if (result.params("var_is_introduced")) =>
      (result, problem.constraints(result))
  }

  def mtch = findSub andThen {
    case (result, constraints) if constraints.size == 2 => constraints.find { c =>
      c.function == 'ge && {
        c.arguments.size == 2 && c.arguments(0) == result
      }
    }
  } andThen {
    case Some(c) => c
  }
  //  val constraints = problem.constraints(v)
  //  if (constraints.size == 2) {
  //    constraints.find { c =>
  //      c.function == 'ge && {
  //        c.arguments.size == 2 && c.arguments(0) == constraint.result
  //      }
  //    }
  //  }

  def compile(subConstraint: CSPOMConstraint, problem: CSPOM, geConstraint: CSPOMConstraint) = {

    problem.removeConstraint(subConstraint);
    problem.removeConstraint(geConstraint);
    problem.removeVariable(subConstraint.result.asInstanceOf[CSPOMVariable])

    val newC = problem.ctr(new CSPOMConstraint(geConstraint.result, 'diffGe, subConstraint.arguments :+ geConstraint.arguments(1)))

    Delta().removed(subConstraint).removed(geConstraint).added(newC)
  }

}
