package concrete.generator.cspompatterns

import cspom.constraint.FunctionalConstraint
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler

/**
 * Transforms x = sub(y, z), [t =] ge(x, k) into [t =] diffGe(y, z, k)
 */
final class DiffGe(val problem: CSPOM) extends ConstraintCompiler {

  override def compileFunctional(subConstraint: FunctionalConstraint) = {
    subConstraint.description == "sub" &&
      subConstraint.result.auxiliary &&
      subConstraint.result.constraints.size == 2 && (
        subConstraint.result.constraints.find { c: CSPOMConstraint =>
          c.description == "ge" && {
            val scope = c match {
              case fGe: FunctionalConstraint => fGe.arguments
              case gGe: GeneralConstraint => gGe.scope
            }
            scope.size == 2 && scope(0) == subConstraint.result
          }
        } map { geConstraint =>

          problem.removeConstraint(subConstraint);
          problem.removeConstraint(geConstraint);
          problem.removeVariable(subConstraint.result)

          geConstraint match {
            case fc: FunctionalConstraint =>
              problem.addConstraint(new FunctionalConstraint(
                fc.result,
                "diffGe",
                subConstraint.arguments :+ fc.arguments(1): _*))
            case _ =>
              problem.addConstraint(new GeneralConstraint("diffGe",
                subConstraint.arguments :+ geConstraint.scope(1): _*))

          }
          true

        } getOrElse (false))

  }

}
