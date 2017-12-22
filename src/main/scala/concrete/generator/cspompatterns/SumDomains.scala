package concrete.generator.cspompatterns

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.linear._
import concrete.generator.SumGenerator
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.VariableCompiler
import cspom.util.IntervalsArithmetic.{Arithmetics, RangeArithmetics}
import cspom.util.{IntInterval, RangeSet}
import cspom.variable._
import cspom.{CSPOMConstraint, UNSATException}

object SumDomains extends VariableCompiler('sum) with LazyLogging {

  def compiler(c: CSPOMConstraint[_]) = throw new IllegalStateException

  override def compilerWEntail(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, _, _) =>

      val (iargs, coef, result, mode) = SumGenerator.readCSPOM(c)

      if (iargs.forall(_.fullyDefined)) {
        (Seq(), false)
      } else {

        val initBound = mode match {
          case SumLE => RangeSet(IntInterval.atMost(result))
          case SumLT => RangeSet(IntInterval.atMost(result - 1))
          case SumEQ => RangeSet(IntInterval.singleton(result))
          case SumNE => RangeSet.allInt -- IntInterval.singleton(result)
        }


        val coefspan = (iargs, coef).zipped.map((a, c) => RangeSet(IntExpression.span(a) * IntInterval.singleton(c))).toIndexedSeq

        val filt = for (i <- iargs.indices) yield {
          val others = iargs.indices
            .filter(_ != i)
            .map(coefspan)
            .foldLeft(initBound)(_ - _)
          iargs(i) -> reduceDomain(iargs(i), others / coef(i))
        }

        val entailed = filt.map(_._2).count(_.searchSpace > 1) <= 1

        // logger.debug((filt, entailed).toString)

        (filt, entailed)

      }

    case CSPOMConstraint(r, _, _, _) => {
      (Seq(r -> BoolExpression.coerce(r)), false)
    }

    // case _ => (Seq(), false)
  }
}

object PseudoBoolDomains extends VariableCompiler('pseudoboolean) {
  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(IntExpression.constSeq(coef), CSPOMSeq(args), CSPOMConstant(result: Int)), _) =>

      val iargs = args.map(BoolExpression.coerce).toIndexedSeq

      val m: String = c.getParam("mode").get

      val initBound = SumMode.withName(m)
        .map {
          case SumLE => IntInterval.atMost(result)
          case SumLT => IntInterval.atMost(result - 1)
          case SumEQ => IntInterval.singleton(result)
          case SumNE => IntInterval.all
        }
        .get

      val coefspan = (iargs, coef).zipped.map((a, c) => BoolExpression.span(a) * IntInterval.singleton(c))

      val filt = for (i <- args.indices) yield {
        val result = iargs.indices
          .filter(_ != i)
          .map(coefspan)
          .foldLeft(initBound)(_ - _) / coef(i)

        if (result.lb > 1 || result.ub < 0) {
          throw new UNSATException()
        } else if (result.lb > 0) {
          args(i) -> reduceDomain(iargs(i), d = true)
        } else if (result.ub < 1) {
          args(i) -> reduceDomain(iargs(i), d = false)
        } else {
          args(i) -> args(i)
        }
      }: (CSPOMExpression[_], CSPOMExpression[_])

      filt
  }

}