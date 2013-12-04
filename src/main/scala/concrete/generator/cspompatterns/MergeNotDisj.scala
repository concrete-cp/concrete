package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMTrue
import cspom.variable.BoolExpression

/**
 * Transforms x = !a, x \/ c \/ ... into !a \/ b \/ c \/ ...
 */
object MergeNotDisj extends ConstraintCompiler {
  type A = CSPOMConstraint
  def mtch(fc: CSPOMConstraint, problem: CSPOM) = fc match {
    case CSPOMConstraint(v: BoolVariable, 'not, Seq(a: BoolExpression), _) if v.params("var_is_introduced") =>
      val ors = problem.constraints(v).toSeq.filter(c =>
        c.function == 'or)

      ors match {
        case Seq(orConstraint) => Some(orConstraint)
        case _ => None
      }
    case _ => None

  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, orConstraint: CSPOMConstraint) = {
    problem.removeConstraint(fc)
    problem.removeConstraint(orConstraint)
    problem.removeVariable(fc.result.asInstanceOf[CSPOMVariable])

    val Seq(a: BoolExpression) = fc.arguments

    val oldOrParams = orConstraint.params.get("revsign").collect {
      case p: Seq[Boolean] => p
    } getOrElse (Seq.fill(orConstraint.arguments.size)(false))

    val (orArgs, orParams) = (orConstraint.arguments zip oldOrParams).filter(_._1 ne fc.result).unzip

    val newScope = a +: orArgs
    val newParams = true +: orParams
    val newConstraint =
      new CSPOMConstraint(orConstraint.result, 'or, newScope, Map("revsign" -> newParams))
    problem.ctr(newConstraint)

    Delta().removed(fc).removed(orConstraint).added(newConstraint)

  }
}
