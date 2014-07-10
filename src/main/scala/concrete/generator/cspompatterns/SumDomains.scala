package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.VariableNames
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval._
import cspom.util.IntInterval
import cspom.util.IntervalsArithmetic._
import cspom.util.RangeSet
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable
import cspom.variable.IntVariable.arithmetics
import cspom.variable.IntVariable.intExpression
import cspom.variable.IntVariable.ranges
import cspom.variable.SimpleExpression
import cspom.variable.CSPOMSeq
import concrete.constraint.semantic.SumMode
import SumMode._

object SumDomains extends VariableCompiler('sum) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(CSPOMSeq(args, _, _), CSPOMConstant(result: Int)), params) =>

      val iargs = args.map(intExpression).toIndexedSeq

      val coef = params.get("coefficients") match {
        case Some(p: Seq[_]) => p.asInstanceOf[Seq[Int]]
        case None => Seq.fill(args.length)(1)
        case _ => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
      }

      val mode = params.get("mode").collect {
        case m: String => SumMode.withName(m)
      }.get

      val initBound = mode match {
        case SumLE => IntInterval.atMost(result)
        case SumLT => IntInterval.atMost(result - 1)
        case SumEQ => IntInterval.singleton(result)
        //case SumNE => ???
      }

      val coefspan = (iargs, coef).zipped.map((a, c) => IntVariable.span(a) * c).toIndexedSeq

      val map = for ((a, i) <- args zip iargs.indices) yield {
        val others = iargs.indices.filter(_ != i).map(coefspan).reduce(_ + _)
        a -> reduceDomain(iargs(i), (initBound - others) / coef(i))
      }

      map.toMap
  }
}