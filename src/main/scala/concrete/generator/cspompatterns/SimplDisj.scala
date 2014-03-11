package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMExpression

/**
 * Removes constants from disjunctions
 */
object SimplDisj extends ConstraintCompilerNoData {

  def matchBool(fc: CSPOMConstraint[_], problem: CSPOM) = fc match {
    case CSPOMConstraint(CSPOMConstant(true), 'or, args, _) if (
      args.exists(_.isInstanceOf[CSPOMConstant[Boolean]])) =>
      true

    case _ => false

  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM) = {
    val expressions = fc.arguments

    val params: Seq[Boolean] = fc.params.get("revsign") match {
      case Some(p: Seq[Boolean]) => p
      case None => Seq.fill(expressions.size)(false)
      case p: Any => throw new IllegalArgumentException(s"Parameters for disjunction must be a sequence of boolean values, not '$p'")
    }

    val validated = (expressions zip params).exists {
      case (CSPOMConstant(c: Boolean), p) => p ^ c
      case _ => false
    }

    val repl = if (validated) {
      Seq()
    } else {
      val (scope, varParams) = (expressions zip params).collect {
        case (v: BoolVariable, p) => (v, p)
      } unzip

      Seq(
        CSPOMConstraint('or, scope, Map("revsign" -> varParams)))
    }

    replaceCtr(fc, repl, problem)
  }

  def selfPropagation = false

}
