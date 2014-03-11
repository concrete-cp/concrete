package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMExpression

/**
 * Transforms x = !a, x \/ c \/ ... into !a \/ b \/ c \/ ...
 */
object MergeNotDisj extends ConstraintCompiler {
  type A = CSPOMConstraint[Boolean]

  override def mtch(fc: CSPOMConstraint[_], problem: CSPOM) = fc match {
    case CSPOMConstraint(v: BoolVariable, 'not, Seq(a: CSPOMExpression[Boolean]), _) if v.hasParam("var_is_introduced") =>
      val ors = problem.constraints(v).toSeq.collect {
        case c: CSPOMConstraint[Boolean] if c.function == 'or => c
      }

      ors match {
        case Seq(orConstraint) => Some(orConstraint)
        case _ => None
      }
    case _ => None

  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, orConstraint: A) = {
    val Seq(a: CSPOMExpression[Boolean]) = fc.arguments

    val oldOrParams = orConstraint.params.get("revsign").collect {
      case p: Seq[Boolean] => p
    } getOrElse (Seq.fill(orConstraint.arguments.size)(false))

    val (orArgs, orParams) = (orConstraint.arguments zip oldOrParams).filter(_._1 ne fc.result).unzip

    val newScope = a +: orArgs
    val newParams = true +: orParams
    val newConstraint =
      new CSPOMConstraint(orConstraint.result, 'or, newScope, Map("revsign" -> newParams))

    replaceCtr(Seq(fc, orConstraint), newConstraint, problem)

  }

  def selfPropagation = true
}