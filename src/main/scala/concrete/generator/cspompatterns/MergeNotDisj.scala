package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq

/**
 * Transforms x = !a, x \/ c \/ ... into !a \/ b \/ c \/ ...
 */
object MergeNotDisj extends ConstraintCompiler {
  type A = CSPOMConstraint[_]

  override def mtch(fc: CSPOMConstraint[_], problem: CSPOM) =
    PartialFunction.condOpt(fc) {
      case CSPOMConstraint(v: CSPOMExpression[_], 'not, Seq(a: CSPOMExpression[_]), _) =>
        problem.constraints(v).toSeq.collect {
          case c: CSPOMConstraint[_] if c.function == 'clause => c
        }
    }
      .collect {
        case Seq(orConstraint) => orConstraint
      }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, clause: A) = {
    val Seq(a: CSPOMExpression[_]) = fc.arguments
    val r = fc.result

    val Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]) = clause.arguments

    val newConstraint =
      new CSPOMConstraint(clause.result, 'clause, Seq(positive, a +: negative))

    replaceCtr(Seq(fc, clause), newConstraint, problem)

  }

  def selfPropagation = true
}
