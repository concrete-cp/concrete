package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import scala.collection.mutable.HashMap
import cspom.variable.CSPOMVariable
import cspom.variable.IntConstant
import cspom.variable.IntVariable

object ConstToVar extends ConstraintCompiler {

  type A = Seq[CSPOMConstant]

  override def matchConstraint(c: CSPOMConstraint) = {
    val constants = c.arguments.collect {
      case c: CSPOMConstant => c
    }

    if (constants.nonEmpty) {
      Some(constants)
    } else {
      None
    }
  }

  val singletons = new HashMap[CSPOMConstant, CSPOMVariable]()

  def compile(constraint: CSPOMConstraint, problem: CSPOM, constants: A) = {
    val newConstraint = constants.foldLeft(constraint) {
      case (constraint, c) => constraint.replacedVar(c,
        singletons.getOrElseUpdate(c, IntVariable.of(
          c match {
            case c: IntConstant => c.value
          })))
    }
    replaceCtr(constraint, newConstraint, problem)
  }

}