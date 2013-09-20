package concrete.generator.cspompatterns

import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
final class MergeEq(private val problem: CSPOM,
  private val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {

  override def compileGeneral(c: GeneralConstraint) = {
    if (c.description == "eq") {
      (for (
        (auxVars, fullVars) <- Some(c.scope.partition { _.auxiliary });
        if (auxVars.nonEmpty)
      ) yield {

        problem.removeConstraint(c)

        /*
         * Generate a new all-equal constraint if more than one variable
         * remains.
         */
        if (fullVars.size > 1) {
          val newConstraint = new GeneralConstraint("eq", fullVars: _*);
          constraints.enqueue(newConstraint)
          problem.addConstraint(newConstraint)
        }

        /*
         * Update the constraints of the problem
         */
        val refVar = if (fullVars.isEmpty) auxVars.head else fullVars.head

        for (aux <- auxVars if aux != refVar) {
          merge(aux, refVar)
          for (c <- aux.constraints) constraints.enqueue(c)
        }
        true

      }) isDefined
    } else false
  }

  private def merge(merged: CSPOMVariable, variable: CSPOMVariable) {
    assume(merged != variable)

    for (d <- merged.domainOption) {
      variable.intersectDomains(d)
    }
    for (d <- variable.domainOption) {
      merged.intersectDomains(d)
    }

    for (c <- merged.constraints) {
      problem.removeConstraint(c);
      problem.addConstraint(c.replacedVar(merged, variable));
    }
    problem.removeVariable(merged);
  }

}
