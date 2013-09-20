package concrete.generator.cspompatterns

import cspom.constraint.{ CSPOMConstraint, GeneralConstraint }
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler

class RemoveAnd(val problem: CSPOM, val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {
  override def compileGeneral(constraint: GeneralConstraint) = {
    if (constraint.description == "and") {
      for (v <- constraint.scope) {
        v.domain = TrueDomain
        for (c <- v.constraints if c != constraint) constraints.enqueue(c)
      }
      problem.removeConstraint(constraint);
      true
    } else false
  }
}
