package concrete.generator.cspompatterns

import concrete.constraint.semantic.SumMode
import concrete.constraint.semantic.SumMode._
import cspom.CSPOMConstraint
import cspom.VariableNames
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval._
import cspom.util.IntInterval
import cspom.util.IntervalsArithmetic._
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

object SumDomains extends VariableCompiler('sum) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(CSPOMSeq(args), CSPOMConstant(result: Int)), params) =>

      val iargs = args.map(IntExpression(_)).toIndexedSeq

      val coef = params.get("coefficients") match {
        case Some(p: Seq[_]) => p.asInstanceOf[Seq[Int]]
        case None            => Seq.fill(args.length)(1)
        case _               => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
      }

      val m: String = params("mode").asInstanceOf[String]

      // Will do nothing (return empty map) if SumMode is SumNE
      PartialFunction.condOpt(SumMode.withName(m)) {
        case SumLE => IntInterval.atMost(result)
        case SumLT => IntInterval.atMost(result - 1)
        case SumEQ => IntInterval.singleton(result)
      }
        .toSeq.flatMap {
          initBound =>

            val coefspan = (iargs, coef).zipped.map((a, c) => IntExpression.span(a) * IntInterval.singleton(c)).toIndexedSeq

            for (i <- args.indices) yield {
              val others = iargs.indices
                .filter(_ != i)
                .map(coefspan)
                .foldLeft(initBound)(_ - _)
              args(i) -> reduceDomain(iargs(i), others / coef(i))
            }
        }
        .toMap
  }
}

object PseudoBoolDomains extends VariableCompiler('pseudoboolean) {
  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(CSPOMSeq(args), CSPOMConstant(result: Int)), params) =>

      val iargs = args.map(BoolExpression(_)).toIndexedSeq

      val coef = params.get("coefficients") match {
        case Some(p: Seq[_]) => p.asInstanceOf[Seq[Int]]
        case None            => Seq.fill(args.length)(1)
        case _               => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
      }

      val m: String = params("mode").asInstanceOf[String]

      // Will do nothing (return empty map) if SumMode is SumNE
      PartialFunction.condOpt(SumMode.withName(m)) {
        case SumLE => IntInterval.atMost(result)
        case SumLT => IntInterval.atMost(result - 1)
        case SumEQ => IntInterval.singleton(result)
      }
        .toSeq.flatMap {
          initBound =>

            val coefspan = (iargs, coef).zipped.map((a, c) => BoolExpression.span(a) * IntInterval.singleton(c)).toIndexedSeq

            for (i <- args.indices) yield {
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
        }
        .toMap
  }

}