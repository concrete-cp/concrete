package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.util.IntervalsArithmetic._
import cspom.variable.CSPOMExpression
import cspom.variable.SimpleExpression
import cspom.variable.CSPOMConstant
import CSPOM.constant
import cspom.variable.IntExpression
import cspom.variable.CSPOMSeq

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
object AbsDiff extends ConstraintCompiler {
  type A = (CSPOMConstraint[Any], CSPOMExpression[_], Set[(CSPOMConstraint[Any], CSPOMExpression[_], CSPOMExpression[_])])

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = c match {
    case absConstraint @ CSPOMConstraint(_, 'abs, Seq(absArg), _) =>

      val addConstraints = problem.deepConstraints(absArg).collect {
        case addConstraint @ CSPOMConstraint(CSPOMConstant(true), 'sum, Seq(
          IntExpression.constSeq(Seq(1, -1, -1)), CSPOMSeq(Seq(a, b, `absArg`)), CSPOMConstant(0)), _) if addConstraint.getParam("mode").contains("eq") =>
          (addConstraint, a, b)
      }

      if (addConstraints.isEmpty) {
        None
      } else {
        Some((absConstraint, absArg, addConstraints.toSet))
      }
    case _ => None
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val (absConstraint, absArg, addConstraints) = data
    val delta = addConstraints.foldLeft(Delta()) {
      case (acc, (addConstraint, a, b)) =>
        /**
         * We have  absConstraint.result = |absArg| and addConstraint.result = absArg + other
         *
         * so absConstraint.result = |addConstraint.result - other|
         */

        val nc = CSPOMConstraint(absConstraint.result, 'absdiff, Seq(a, b))
        val delta = acc ++ addCtr(nc, problem) //acc.added(problem.ctr(nc))
        /**
         *  Remove addConstraint if absArg is not referenced
         */
        if (problem.namesOf(absArg).isEmpty) {
          delta ++ removeCtr(addConstraint, problem)
        } else {
          delta
        }
    }

    if (problem.constraints(absArg).size == 1) {
      delta ++ removeCtr(absConstraint, problem)
    } else {
      delta
    }

  }

  def selfPropagation = false
}
