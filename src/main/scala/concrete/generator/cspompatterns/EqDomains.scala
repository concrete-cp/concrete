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
import cspom.variable.CSPOMSeq

object EqDomains extends VariableCompiler('eq) {

  def compiler(c: CSPOMConstraint[_]) = ???

  override def compilerWEntail(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(IntExpression(i0), IntExpression(i1)), params) =>
      require(!params.contains("neg") && !params.contains("offset"), "Neg/offset parameters are deprecated")
      //      val neg: Boolean = params.get("neg").map { case n: Boolean => n }.getOrElse(false)
      //      val offset: Int = params.get("offset").map { case o: Int => o }.getOrElse(0)
      //
      //      val negFactor = if (neg) -1 else 1

      val br = BoolExpression.coerce(r)

      val intersect = i0 & i1

      val res =
        if (intersect.isEmpty) {
          reduceDomain(br, false)
        } else {
          (i0, i1) match {
            case (CSPOMConstant(i), CSPOMConstant(j)) => reduceDomain(br, i == j)
            case _                                    => br
          }
        }

      val ri0 = if (res.isTrue) {
        reduceDomain(i0, intersect)
      } else {
        i0
      }

      val ri1 = if (res.isTrue) {
        reduceDomain(i1, intersect)
      } else {
        i1
      }

      (Map(r -> res, i0 -> ri0, i1 -> ri1), CSPOMSeq(res, ri0, ri1).searchSpace == 1)
    case _ => (Map(), false)
  }
}