package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable

/**
 * Transforms x = a \/ b, x \/ c \/ ... into a \/ b \/ c \/ ...
 */
object MergeDisj extends ConstraintCompiler {
  type A = CSPOMConstraint
  def mtch(fc: CSPOMConstraint, problem: CSPOM) = fc match {
    case CSPOMConstraint(v: CSPOMVariable, 'or, _, _) if v.params("var_is_introduced") =>
      problem.constraints(v).filter(c => c.function == 'or && (c ne fc)) match {
        case Seq(orConstraint) => Some(orConstraint)
        case _ => None
      }
    case _ => None

  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, orConstraint: CSPOMConstraint) = {
    problem.removeConstraint(fc)
    problem.removeConstraint(orConstraint)
    problem.removeVariable(fc.result.asInstanceOf[CSPOMVariable])

    val newScope = fc.arguments ++ orConstraint.scope.filter(_ ne fc.result)
    val newConstraint = new CSPOMConstraint('or, newScope: _*)
    problem.ctr(newConstraint)

    new Delta(Seq(fc, orConstraint), newConstraint.scope)

  }

}
