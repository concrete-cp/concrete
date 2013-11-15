package concrete.generator.cspompatterns

import cspom.variable.CSPOMVariable
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.compiler.Delta
import cspom.variable.CSPOMTrue
import cspom.variable.IntVariable
import cspom.variable.IntConstant
import cspom.variable.CSPOMExpression
import cspom.variable.IntDomain
import cspom.variable.CSPOMConstant
import cspom.compiler.Delta
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompiler
import cspom.variable.IntVariable
import cspom.CSPOMConstraint

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object MergeEq extends ConstraintCompiler {

  type A = (List[CSPOMVariable], List[CSPOMVariable], List[CSPOMConstant])

  @annotation.tailrec
  private def partition(
    s: List[CSPOMExpression],
    aux: List[CSPOMVariable] = Nil,
    full: List[CSPOMVariable] = Nil,
    const: List[CSPOMConstant] = Nil): (List[CSPOMVariable], List[CSPOMVariable], List[CSPOMConstant]) =
    s match {
      case Nil => (aux, full, const)
      case ::(a: CSPOMVariable, tail) if a.params("var_is_introduced") => partition(tail, a :: aux, full, const)
      case ::(f: CSPOMVariable, tail) => partition(tail, aux, f :: full, const)
      case ::(c: CSPOMConstant, tail) => partition(tail, aux, full, c :: const)
      case o => throw new IllegalArgumentException(o.toString)
    }

  def mtch(c: CSPOMConstraint, problem: CSPOM) = {
    if (c.function == 'eq && c.result == CSPOMTrue) {

      val (aux, full, const) = partition(c.arguments.toList)
      if (aux.nonEmpty) {
        Some((aux, full, const))
      } else None
    } else {
      None
    }
  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, data: A) = {
    val (auxVars, fullVars, const) = data

    problem.removeConstraint(constraint)
    /**
     * Generate a new all-equal constraint if more than one variable
     * remains.
     */
    if (fullVars.size > 1) {
      problem.ctr(new CSPOMConstraint('eq, fullVars.toSeq: _*))
    }

    var delta = Delta()
    /**
     * Update the constraints of the problem
     */
    const match {
      case c :: tail =>
        require(tail.forall(_ == c))

        val newFullVars = fullVars.map {
          v =>
            val nv = v.intersected(c)
            delta ++= replaceVars(Seq(v), nv, problem)
            nv
        }

        delta ++= replaceVars(auxVars, c, problem)

      case Nil =>
        val l = fullVars ++ auxVars
        var merged: CSPOMExpression = l.head
        for (v <- l.tail) {
          merged = merged.intersected(v)
        }
        //reduce(_.intersected(_))

        /**
         * Tighten fullVars' domain
         */

        val newFullVars = fullVars.map {
          v =>
            val nv = v.intersected(merged)
            delta ++= replaceVars(Seq(v), nv, problem)
            nv
        }

        /**
         * Replacing aux variables by a single one (full var if available)
         */
        val refVar = newFullVars.headOption.getOrElse(auxVars.head)

        delta ++= replaceVars(auxVars.distinct, refVar, problem)

    }
    require(fullVars.forall(v => problem.variable(v.name).nonEmpty))

    delta

  }

}
