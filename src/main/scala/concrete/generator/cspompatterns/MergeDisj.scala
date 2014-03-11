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
import cspom.variable.CSPOMExpression

/**
 * Transforms x = a \/ b, x \/ c \/ ... into a \/ b \/ c \/ ...
 */
object MergeDisj extends ConstraintCompiler {
  type A = CSPOMConstraint[CSPOMConstant[Boolean]]

  override def mtch(fc: CSPOMConstraint[_], problem: CSPOM) = fc match {
    case CSPOMConstraint(v: CSPOMExpression[Boolean], 'or, _, _) =>
      problem.constraints(v).toSeq.collect {
        case orConstraint @ CSPOMConstraint(CSPOMConstant(true), 'or, _, _) if (orConstraint ne fc) =>
          orConstraint.asInstanceOf[CSPOMConstraint[CSPOMConstant[Boolean]]]
      } match {
        case Seq(orConstraint) => Some(orConstraint)
        case _ => None
      }
    case _ => None

  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, orConstraint: A) = {

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
      CSPOMConstraint('or, newScope, Map("revsign" -> newParams))

    replaceCtr(Seq(fc, orConstraint), newConstraint, problem)

  }
  def selfPropagation = true
}
