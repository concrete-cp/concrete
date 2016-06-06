package concrete.generator.cspompatterns

import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.SumLE
import concrete.constraint.linear.SumLT
import concrete.constraint.linear.SumMode
import concrete.constraint.linear.SumNE
import cspom.CSPOMConstraint
import cspom.UNSATException
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval
import cspom.variable.BoolExpression
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.util.IntervalsArithmetic.RangeArithmetics
import cspom.variable.IntVariable
import cspom.util.RangeSet
import cspom.util.Infinitable

object SumDomains extends VariableCompiler('sum) {

  def compiler(c: CSPOMConstraint[_]) = throw new IllegalStateException

  override def compilerWEntail(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(IntExpression.constSeq(coef), CSPOMSeq(args), CSPOMConstant(result: Int)), _) =>

      val iargs = args.map(IntExpression.coerce(_)).toIndexedSeq

      val m: String = c.getParam("mode").get

      val initBound: RangeSet[Infinitable] = SumMode.withName(m)
        .map {
          case SumLE => RangeSet(IntInterval.atMost(result))
          case SumLT => RangeSet(IntInterval.atMost(result - 1))
          case SumEQ => RangeSet(IntInterval.singleton(result))
          case SumNE => RangeSet.allInt -- IntInterval.singleton(result)
        }
        .get

      val coefspan = (iargs, coef).zipped.map((a, c) => RangeSet(IntExpression.span(a) * IntInterval.singleton(c))).toIndexedSeq

      val filt = for (i <- args.indices) yield {
        val others = iargs.indices
          .filter(_ != i)
          .map(coefspan)
          .foldLeft(initBound)(_ - _)
        args(i) -> reduceDomain(iargs(i), others / coef(i))
      }

      val entailed = args.collect { case IntVariable(e) => e }.size == 1

      (filt, entailed)

    case _ => (Seq(), false)
  }
}

object PseudoBoolDomains extends VariableCompiler('pseudoboolean) {
  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(IntExpression.constSeq(coef), CSPOMSeq(args), CSPOMConstant(result: Int)), _) =>

      val iargs = args.map(BoolExpression.coerce(_)).toIndexedSeq

      val m: String = c.getParam("mode").get

      val initBound = SumMode.withName(m)
        .map {
          case SumLE => IntInterval.atMost(result)
          case SumLT => IntInterval.atMost(result - 1)
          case SumEQ => IntInterval.singleton(result)
          case SumNE => IntInterval.all
        }
        .get

      val coefspan = (iargs, coef).zipped.map((a, c) => BoolExpression.span(a) * IntInterval.singleton(c)).toIndexedSeq

      val filt = for (i <- args.indices) yield {
        val result = iargs.indices
          .filter(_ != i)
          .map(coefspan)
          .foldLeft(initBound)(_ - _) / coef(i)

        if (result.lb > 1 || result.ub < 0) {
          throw new UNSATException()
        } else if (result.lb > 0) {
          args(i) -> reduceDomain(iargs(i), true)
        } else if (result.ub < 1) {
          args(i) -> reduceDomain(iargs(i), false)
        } else {
          args(i) -> args(i)
        }
      }: (CSPOMExpression[_], CSPOMExpression[_])

      filt
  }

}