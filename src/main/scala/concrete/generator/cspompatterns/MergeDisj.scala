package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta

/**
 * Transforms x = a \/ b, x \/ c \/ ... into a \/ b \/ c \/ ...
 */
object MergeDisj extends ConstraintCompiler {
  type A = Seq[CSPOMConstraint]
  def mtch(fc: CSPOMConstraint, problem: CSPOM) = {
    if (fc.function == "or") {
      fc.result match {
        case v: CSPOMVariable if v.params("var_is_introduced") =>
          val constraints = problem.constraints(v)
          if (constraints.size == 2) {
            val orConstraints = constraints.filter(_.function == "or")
            if (orConstraints.nonEmpty) {
              Some(orConstraints)
            } else {
              None
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

  def compile(fc: CSPOMConstraint, problem: CSPOM, orConstraints: Seq[CSPOMConstraint]) = {
    orConstraints.foldLeft(Delta()) { (acc, orConstraint) =>
      problem.removeConstraint(fc)
      problem.removeConstraint(orConstraint)
      problem.removeVariable(fc.result.asInstanceOf[CSPOMVariable])

      val newScope = fc.arguments ++ orConstraint.scope.filter(_ ne fc.result)
      val newConstraint = new CSPOMConstraint("or", newScope: _*)
      problem.addConstraint(newConstraint)

      acc ++ new Delta(Seq(fc, orConstraint), newConstraint.scope)
    }
  }

}
