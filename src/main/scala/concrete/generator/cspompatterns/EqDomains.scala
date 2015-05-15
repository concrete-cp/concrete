package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.SimpleExpression
import cspom.variable.BoolExpression

object EqDomains extends VariableCompiler('eq) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(IntExpression(i0), IntExpression(i1)), params) =>
      val neg: Boolean = params.get("neg").map { case n: Boolean => n }.getOrElse(false)
      val offset: Int = params.get("offset").map { case o: Int => o }.getOrElse(0)

      val negFactor = if (neg) -1 else 1

      val br = BoolExpression.coerce(r)
      val ii0 = IntExpression.coerce(i0)
      val ii1 = IntExpression.coerce(i1)

      val intersect = (ii0 * IntInterval.singleton(negFactor) + IntInterval.singleton(offset)) & ii1

      val res = if (intersect.isEmpty) {
        reduceDomain(br, false)
      } else (ii0, ii1) match {
        case (CSPOMConstant(i), CSPOMConstant(j)) => reduceDomain(br, i * negFactor + offset == j)
        case _                                    => br
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