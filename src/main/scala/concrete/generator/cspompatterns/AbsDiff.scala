package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.Delta
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import ConstraintCompiler._
/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
object AbsDiff extends ConstraintCompiler {
  type A = (CSPOMConstraint[Any], CSPOMExpression[_], Set[(CSPOMConstraint[Any], Seq[CSPOMExpression[_]])])

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = c match {
    case absConstraint @ CSPOMConstraint(_, 'abs, Seq(absArg), _) =>
      
     // println(problem.deepConstraints(absArg))

      val addConstraints = problem.deepConstraints(absArg).collect {
        case addConstraint @ CSPOMConstraint(CSPOMConstant(true), 'sum, Seq(
          IntExpression.constSeq(coefs), CSPOMSeq(args), CSPOMConstant(0)), _) if addConstraint.getParam("mode").contains("eq") &&
          coefs.sorted == Seq(-1, -1, 1) && (args zip coefs).find(_._1 == absArg).exists(_._2 == -1) =>
          (addConstraint, args.filter(_ != absArg))
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
      case (acc, (addConstraint, other)) =>

        /*
         * We have  absConstraint.result = |absArg| and absArg = other
         *
         * so absConstraint.result = |other|
         */

        val nc = CSPOMConstraint(absConstraint.result, 'absdiff, other)
        val delta = acc ++ addCtr(nc, problem) //acc.added(problem.ctr(nc))
        /*
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
