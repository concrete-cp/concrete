package concrete.generator.cspompatterns

import com.typesafe.scalalogging.LazyLogging
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.VariableCompiler
import cspom.util.IntervalsArithmetic.Arithmetics
import cspom.variable.{IntExpression, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}

object AbsDiffDomains extends VariableCompiler('absdiff) with LazyLogging {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(ir: SimpleExpression[_], _, Seq(ii0: SimpleExpression[_], ii1: SimpleExpression[_]), _) =>
      val r = IntExpression.coerce(ir)
      val i0 = IntExpression.coerce(ii0)
      val i1 = IntExpression.coerce(ii1)

      val rSpan = IntExpression.span(r)
      val i0Span = IntExpression.span(i0)
      val i1Span = IntExpression.span(i1)

      Seq(
        ir -> reduceDomain(r, (i0Span - i1Span).abs),
        ii0 -> reduceDomain(i0, (i1Span + rSpan) span (i1Span - rSpan)),
        ii1 -> reduceDomain(i1, (i0Span + rSpan) span (i0Span - rSpan)))

    case _ => throw new IllegalArgumentException
  }

  override def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    logger.info {
      val e = problem.referencedExpressions
      val ct = e.count(_.fullyDefined)
      s"$ct/${e.size}"
    }

    super.compile(c, problem, data)
  }
}