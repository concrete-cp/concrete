package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import scala.collection.mutable.HashMap
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable

object ConstToVar extends ConstraintCompiler {

  type A = Seq[CSPOMConstant[_]]

  override def matchConstraint(c: CSPOMConstraint[_]) = {
    val constants = c.arguments.collect {
      case c: CSPOMConstant[_] => c
    }

    if (constants.nonEmpty) {
      Some(constants)
    } else {
      None
    }
  }

  val singletons = new HashMap[CSPOMConstant[_], CSPOMVariable[_]]()

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, constants: A) = {
    val newConstraint = constants.foldLeft(constraint) {
      case (constraint, c) => constraint.replacedVar(c,
        singletons.getOrElseUpdate(c, IntVariable.of {
          val CSPOMConstant(value: Int) = c
          value
        }))
    }
    replaceCtr(constraint, newConstraint, problem)
  }
  def selfPropagation = false
}