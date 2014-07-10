package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import scala.collection.mutable.HashMap
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.compiler.Delta

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

    val deltas: Seq[Delta] = for (c <- constants.distinct) yield {
      val singleton = singletons.getOrElseUpdate(c, c match {
        case CSPOMConstant(value: Int) => IntVariable.ofSeq(value)
        case CSPOMConstant(value: Boolean) => IntVariable.ofSeq((if (value) 1 else 0))
      })

      replace(Seq(c), singleton, problem)
    }
    deltas.reduceLeft(_ ++ _)
  }
  def selfPropagation = false
}