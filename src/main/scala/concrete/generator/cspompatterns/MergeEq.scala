package concrete.generator.cspompatterns

import cspom.variable.CSPOMVariable
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.compiler.Delta
import cspom.variable.CSPOMTrue
import cspom.variable.IntVariable

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object MergeEq extends ConstraintCompiler {

  type A = (Set[IntVariable], Set[IntVariable])

  def mtch(c: CSPOMConstraint, problem: CSPOM) = {
    if (c.function == "eq" && c.result == CSPOMTrue) {
      val intVars = c.scope.collect {
        case v: IntVariable => v
      }

      if (intVars == c.scope) {
        val (auxVars, fullVars) = intVars.partition(_.params("var_is_introduced"))
        if (auxVars.nonEmpty) {
          Some((auxVars, fullVars))
        } else {
          None
        }
      } else {
        None
      }

    } else {
      None
    }
  }

  def compile(c: CSPOMConstraint, problem: CSPOM, data: A) = {
    val (auxVars, fullVars) = data

    problem.removeConstraint(c)

    /**
     * Generate a new all-equal constraint if more than one variable
     * remains.
     */
    if (fullVars.size > 1) {
      val newConstraint = new CSPOMConstraint("eq", fullVars.toSeq: _*);
      problem.addConstraint(newConstraint)
    }

    /**
     * Update the constraints of the problem
     */
    val mergedDom = (fullVars ++ auxVars).map(_.domain).reduce(_ intersect _)

    val refVar = fullVars.headOption.getOrElse(auxVars.head)

    val mergedVar = new IntVariable(refVar.name, mergedDom)

    (auxVars + refVar).foldLeft(Delta()) { (acc, v) =>
      acc ++ replaceVar(v, mergedVar, problem)
    }

  }

}
