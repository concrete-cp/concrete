package concrete.generator.cspompatterns

import concrete.constraint.semantic.SumMode
import concrete.constraint.semantic.SumMode._
import cspom.CSPOMConstraint
import cspom.VariableNames
import cspom.compiler.VariableCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.SimpleExpression
import cspom.util.Interval
import scala.PartialFunction
import cspom.variable.BoolExpression
import cspom.variable.BoolVariable
import cspom.UNSATException
import cspom.CSPOM
import cspom.compiler.Delta
import cspom.util.RangeSet
import cspom.util.IntInterval

object SumDomains extends VariableCompiler('sum) {
  def compiler(c: CSPOMConstraint[_]) = ???
  override def compilerWEntail(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(IntExpression.constSeq(coef), CSPOMSeq(args), CSPOMConstant(result: Int)), _) =>

      val iargs = args.map(IntExpression.coerce(_)).toIndexedSeq

      val m: String = c.getParam("mode").get

      val initBound = SumMode.withName(m) match {
        case SumLE => RangeSet(IntInterval.atMost(result))
        case SumLT => RangeSet(IntInterval.atMost(result - 1))
        case SumEQ => RangeSet(IntInterval.singleton(result))
        case SumNE => RangeSet.allInt -- IntInterval.singleton(result)
      }

      val coefspan = (iargs, coef).zipped.map((a, c) => RangeSet(IntExpression.span(a)) * RangeSet(IntInterval.singleton(c))).toIndexedSeq

      val filt = for (i <- args.indices) yield {
        val others = iargs.indices
          .filter(_ != i)
          .map(coefspan)
          .foldLeft(initBound)(_ - _)
        args(i) -> reduceDomain(iargs(i), others / coef(i))
      }

      (filt.toMap, args.size == 1)

    case _ => (Map(), false)
  }
}

object PseudoBoolDomains extends VariableCompiler('pseudoboolean) {
  def compiler(c: CSPOMConstraint[_]) = ???
  override def compilerWEntail(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(IntExpression.constSeq(coef), CSPOMSeq(args), CSPOMConstant(result: Int)), _) =>

      val iargs = args.map(BoolExpression.coerce(_)).toIndexedSeq

      val m: String = c.getParam("mode").get

      val initBound = SumMode.withName(m) match {
        case SumLE => RangeSet(IntInterval.atMost(result))
        case SumLT => RangeSet(IntInterval.atMost(result - 1))
        case SumEQ => RangeSet(IntInterval.singleton(result))
        case SumNE => RangeSet.allInt -- IntInterval.singleton(result)
      }

      val coefspan = (iargs, coef).zipped.map((a, c) => RangeSet(BoolExpression.span(a)) * RangeSet(IntInterval.singleton(c))).toIndexedSeq

      val filt = for (i <- args.indices) yield {
        val result = iargs.indices
          .filter(_ != i)
          .map(coefspan)
          .foldLeft(initBound)(_ - _) / coef(i)

        if (result.lowerBound > 1 || result.upperBound < 0) {
          throw new UNSATException()
        } else if (result.lowerBound > 0) {
          args(i) -> reduceDomain(iargs(i), true)
        } else if (result.upperBound < 1) {
          args(i) -> reduceDomain(iargs(i), false)
        } else {
          args(i) -> args(i)
        }
      }: (CSPOMExpression[_], CSPOMExpression[_])

      (filt.toMap, args.size == 1)
  }

}