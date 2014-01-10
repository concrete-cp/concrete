package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMTrue

/**
 * Transforms x = a \/ b, x \/ c \/ ... into a \/ b \/ c \/ ...
 */
object MergeDisj extends ConstraintCompiler {
  type A = CSPOMConstraint

  val otherOr: PartialFunction[(CSPOMConstraint, CSPOM), Seq[CSPOMConstraint]] = {
    case (fc @ CSPOMConstraint(v: CSPOMVariable, 'or, _, _), problem) if v.params("var_is_introduced") =>
      problem.constraints(v).toSeq.filter(c =>
        c.result == CSPOMTrue && c.function == 'or && (c ne fc))
  }

  def mtch = otherOr andThen {
    case Seq(orConstraint) => orConstraint
  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, orConstraint: CSPOMConstraint) = {
    problem.removeConstraint(fc)
    problem.removeConstraint(orConstraint)
    problem.removeVariable(fc.result.asInstanceOf[CSPOMVariable])

    val fcParams = fc.params.get("revsign").collect {
      case p: Seq[Boolean] => p
    } getOrElse (Seq.fill(fc.arguments.size)(false))

    val oldOrParams = orConstraint.params.get("revsign").collect {
      case p: Seq[Boolean] => p
    } getOrElse (Seq.fill(orConstraint.arguments.size)(false))

    val (orArgs, orParams) = (orConstraint.arguments zip oldOrParams).filter(_._1 ne fc.result).unzip

    val newScope = fc.arguments ++ orArgs
    val newParams = fcParams ++ orParams
    val newConstraint =
      new CSPOMConstraint(CSPOMTrue, 'or, newScope, Map("revsign" -> newParams))
    problem.ctr(newConstraint)

    Delta().removed(fc).removed(orConstraint).added(newConstraint)

  }

}
