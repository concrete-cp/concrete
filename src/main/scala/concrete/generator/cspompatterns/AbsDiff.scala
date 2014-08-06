package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.util.IntervalsArithmetic._
import cspom.variable.CSPOMExpression
import cspom.variable.SimpleExpression

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
object AbsDiff extends ConstraintCompiler {
  type A = (CSPOMConstraint[Any], SimpleExpression[Any], Set[CSPOMConstraint[Any]])

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = c match {
    case absConstraint @ CSPOMConstraint(_, 'abs, Seq(absArg: SimpleExpression[_]), _) =>
      val addConstraints = problem.constraints(absArg).collect {
        case addConstraint @ CSPOMConstraint(_, 'add, addArgs, _) if addArgs.size == 2 && addArgs.contains(absArg) =>
          addConstraint
      }

      if (addConstraints.isEmpty) {
        None
      } else {
        Some((absConstraint, absArg, addConstraints))
      }
    case _ => None
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val (absConstraint, absArg, addConstraints) = data
    val delta = addConstraints.foldLeft(Delta()) {
      case (acc, addConstraint) =>
        val Seq(other: SimpleExpression[_]) = addConstraint.arguments.filterNot(_ eq absArg)

        /**
         * We have  absConstraint.result = |absArg| and addConstraint.result = absArg + other
         *
         * so absConstraint.result = |addConstraint.result - other|
         */

        val nc = CSPOMConstraint(absConstraint.result, 'absdiff, Seq(addConstraint.result, other))
        val delta = acc.added(problem.ctr(nc))
        /**
         *  Remove addConstraint if absArg is not referenced
         */
        if (problem.namesOf(absArg).isEmpty) {
          problem.removeConstraint(addConstraint)
          delta.removed(addConstraint)
        } else {
          delta
        }
    }

    if (problem.constraints(absArg).size == 1) {
      problem.removeConstraint(absConstraint)
      delta.removed(absConstraint)
    } else {
      delta
    }

  }

  def selfPropagation = false
}
