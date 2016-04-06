package concrete.generator.cspompatterns

import scala.reflect.runtime.universe

import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.SumLE
import concrete.constraint.linear.SumLT
import concrete.constraint.linear.SumMode
import concrete.constraint.linear.SumNE
import cspom.CSPOMConstraint
import cspom.UNSATException
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval
import cspom.util.RangeSet
import cspom.variable.BoolExpression
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.arithmetics

object SumDomains extends VariableCompiler('sum) {
  def compiler(c: CSPOMConstraint[_]) = ???
  override def compilerWEntail(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(IntExpression.constSeq(coef), CSPOMSeq(args), CSPOMConstant(result: Int)), _) =>

      val iargs = args.map(IntExpression.coerce(_)).toIndexedSeq

      val m: String = c.getParam("mode").get

      val initBound = SumMode.withName(m)
        .map {
          case SumLE => RangeSet(IntInterval.atMost(result))
          case SumLT => RangeSet(IntInterval.atMost(result - 1))
          case SumEQ => RangeSet(IntInterval.singleton(result))
          case SumNE => RangeSet.allInt -- IntInterval.singleton(result)
        }
        .get

      val coefspan = (iargs, coef).zipped.map((a, c) => RangeSet(IntExpression.span(a)) * RangeSet(IntInterval.singleton(c))).toIndexedSeq

      val filt = for (i <- args.indices) yield {
        val others = iargs.indices
          .filter(_ != i)
          .map(coefspan)
          .foldLeft(initBound)(_ - _)
        args(i) -> reduceDomain(iargs(i), others / coef(i))
      }

      (filt, args.size == 1)

    case _ => (Seq(), false)
  }
}

object NeDomains extends VariableCompiler('eq) {
  def compiler(c: CSPOMConstraint[_]) = ???
  override def compilerWEntail(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(false), _, Seq(IntExpression(a), IntExpression(b)), _) =>

      val initBound = RangeSet.allInt -- IntInterval.singleton(0)

      val na = reduceDomain(a, initBound + IntExpression.implicits.ranges(b))
      val nb = reduceDomain(b, initBound + IntExpression.implicits.ranges(a))

      (
        Seq(a -> na, b -> nb),
        na.searchSpace <= 1 || nb.searchSpace <= 1)

    case _ => (Seq(), false)
  }
}

object PseudoBoolDomains extends VariableCompiler('pseudoboolean) {
  def compiler(c: CSPOMConstraint[_]) = ???
  override def compilerWEntail(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(IntExpression.constSeq(coef), CSPOMSeq(args), CSPOMConstant(result: Int)), _) =>

      val iargs = args.map(BoolExpression.coerce(_)).toIndexedSeq

      val m: String = c.getParam("mode").get

      val initBound = SumMode.withName(m)
        .map {
          case SumLE => RangeSet(IntInterval.atMost(result))
          case SumLT => RangeSet(IntInterval.atMost(result - 1))
          case SumEQ => RangeSet(IntInterval.singleton(result))
          case SumNE => RangeSet.allInt -- IntInterval.singleton(result)
        }
        .get

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

      (filt, args.size == 1)
  }

}