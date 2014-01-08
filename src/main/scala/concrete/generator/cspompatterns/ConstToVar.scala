package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import scala.collection.mutable.HashMap
import cspom.variable.CSPOMVariable
import cspom.variable.IntConstant

object ConstToVar extends ConstraintCompiler {

  type A = Seq[CSPOMConstant]

  def mtch = constraintMatch andThen {
    case constraint => constraint.arguments.collect {
      case c: CSPOMConstant => c
    }
  } andThen {
    case constants if constants.nonEmpty => constants
  }

  val singletons = new HashMap[CSPOMConstant, CSPOMVariable]()

  def compile(constraint: CSPOMConstraint, problem: CSPOM, constants: A) = {
    val newConstraint = constants.foldLeft(constraint) {
      case (constraint, c) => constraint.replacedVar(c,
        singletons.getOrElseUpdate(c, CSPOMVariable.ofInt(
          c match {
            case c: IntConstant => c.value
          })))
    }
    replaceCtr(constraint, newConstraint, problem)
  }

}