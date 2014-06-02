package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntVariable.arithmetics
import cspom.variable.IntVariable.ranges
import cspom.variable.SimpleExpression

object EqDomains extends VariableCompiler('eq) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i0: SimpleExpression[_], i1: SimpleExpression[_]), params) =>
      val neg: Boolean = params.get("neg").map { case n: Boolean => n }.getOrElse(false)
      val negFactor = if (neg) -1 else 1
      val offset: Int = params.get("offset").map { case o: Int => o }.getOrElse(0)

      val br = booleanExpression(r)

      val intersect = i0 intersected i1

      Map(
        r -> (if (intersect.isEmpty) CSPOMConstant(true) else r),
        i0 -> intersect,
        i1 -> intersect)
  }
}