package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression

/**
 * @author vion
 */
object AtLeastDomains extends VariableCompiler('atLeast) {
  def compiler(c: CSPOMConstraint[_]): Seq[(CSPOMExpression[_], CSPOMExpression[_])] = {
    val r = c.arguments(0)
    Seq(r -> reduceDomain(IntExpression.coerce(r), IntInterval.atLeast(0)))
  }
}

object AtMostDomains extends VariableCompiler('atMost) {
  def compiler(c: CSPOMConstraint[_]): Seq[(CSPOMExpression[_], CSPOMExpression[_])] = {
    val CSPOMConstraint(_, _, Seq(r, _, CSPOMSeq(args)), _) = c

    Seq(r -> reduceDomain(IntExpression.coerce(r), IntInterval.atMost(args.size)))
  }
}