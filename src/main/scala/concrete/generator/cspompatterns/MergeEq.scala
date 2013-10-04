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

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object MergeEq extends ConstraintCompiler {

  type A = (List[IntVariable], List[IntVariable], List[IntConstant])

  private def partition(s: List[CSPOMExpression], aux: List[IntVariable] = Nil, full: List[IntVariable] = Nil, const: List[IntConstant] = Nil): (List[IntVariable], List[IntVariable], List[IntConstant]) =
    s match {
      case Nil => (aux, full, const)
      case ::(a: IntVariable, tail) if a.params("var_is_introduced") => partition(tail, a :: aux, full, const)
      case ::(f: IntVariable, tail) => partition(tail, aux, f :: full, const)
      case ::(c: IntConstant, tail) => partition(tail, aux, full, c :: const)
      case _ => throw new IllegalArgumentException
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

  def compile(c: CSPOMConstraint, problem: CSPOM, data: A) = {
    val (auxVars, fullVars, const) = data

    problem.removeConstraint(c)

    /**
     * Generate a new all-equal constraint if more than one variable
     * remains.
     */
    if (fullVars.size > 1) {
      val newConstraint = new CSPOMConstraint('eq, fullVars.toSeq: _*);
      problem.addConstraint(newConstraint)
    }

    /**
     * Update the constraints of the problem
     */
    const match {
      case c :: tail =>
        require(tail.forall(_ == c))
        var delta = Delta()
        val mergedDom = IntDomain.of(c.value)
        val newFullVars = fullVars.map {
          v =>
            val nv = new IntVariable(v.name, mergedDom)
            delta ++= replaceVars(Seq(v), nv, problem)
            nv
        }

        delta ++ replaceVars(auxVars, c, problem)

      case Nil =>
        val mergedDom = (fullVars ++ auxVars).map(_.domain).reduce(_ intersect _)

        /**
         * Tighten fullVars' domain
         */
        var delta = Delta()

        val newFullVars = fullVars.map {
          v =>
            val nv = new IntVariable(v.name, mergedDom)
            delta ++= replaceVars(Seq(v), nv, problem)
            nv
        }

        /**
         * Replacing aux variables by a single one (full var if available)
         */
        val refVar = newFullVars.headOption.getOrElse(auxVars.head)

        val mergedVar = new IntVariable(refVar.name, mergedDom)

        delta ++ replaceVars((auxVars :+ refVar).distinct, mergedVar, problem)
    }

  }

}
