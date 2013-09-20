package concrete.generator.cspompatterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.CSPOMConstraint
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler

/**
 * Transforms x = a \/ b, x \/ c \/ ... into a \/ b \/ c \/ ...
 */
final class MergeDisj(
  private val problem: CSPOM,
  private val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {

  override def compileFunctional(fc: FunctionalConstraint)  = {
    if (fc.description == "or" &&
      fc.result.auxiliary &&
      fc.result.constraints.size == 2) {

      (for (
        orConstraint <- fc.result.generalConstraints if orConstraint.description == "or"
      ) yield {
        problem.removeConstraint(fc)
        problem.removeConstraint(orConstraint)
        problem.removeVariable(fc.result)
        
        val newScope = fc.arguments ++ orConstraint.scope.filter(_ ne fc.result)
        val newConstraint = new GeneralConstraint("or", newScope: _*)
        problem.addConstraint(newConstraint)
        
//        for (v <- newScope; c <- v.constraints if c != newConstraint) {
//          constraints.enqueue(c)
//        }
      }) contains (true)

    } else false

  } 

}
