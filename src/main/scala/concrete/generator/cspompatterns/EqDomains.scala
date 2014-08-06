package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntVariable.arithmetics
import cspom.variable.IntVariable.ranges
import cspom.variable.SimpleExpression
import cspom.util.IntervalsArithmetic._
import cspom.variable.BoolVariable
import cspom.variable.IntVariable.intExpression
import cspom.variable.CSPOMConstant
import cspom.util.IntInterval._
import cspom.variable.IntVariable
import cspom.util.IntInterval
import cspom.variable.CSPOMExpression
import cspom.VariableNames

object EqDomains extends VariableCompiler('eq) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _,
      Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]),
      params) if (IntVariable.isInt(i0) || IntVariable.isInt(i1)) =>
      val neg: Boolean = params.get("neg").map { case n: Boolean => n }.getOrElse(false)
      val offset: Int = params.get("offset").map { case o: Int => o }.getOrElse(0)

      val negFactor = if (neg) -1 else 1

      val br = BoolVariable.boolExpression(r)
      val ii0 = intExpression(i0)
      val ii1 = intExpression(i1)
      val intersect = (ii0 * IntInterval.singleton(negFactor) + IntInterval.singleton(offset)) & ii1

      val res = if (intersect.isEmpty) {
        reduceDomain(br, false)
      } else (ii0, ii1) match {
        case (CSPOMConstant(i), CSPOMConstant(j)) => reduceDomain(br, i * negFactor + offset == j)
        case _ => br
      }

      val ri0 = if (res.isTrue) {
        reduceDomain(ii0, (intersect - IntInterval.singleton(offset)) * IntInterval.singleton(negFactor))
      } else {
        ii0
      }

      val ri1 = if (res.isTrue) {
        reduceDomain(ii1, intersect)
      } else {
        ii1
      }

      Map(r -> res, i0 -> ri0, i1 -> ri1)
    case _ => Map()
  }
}