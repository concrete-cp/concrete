package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.SimpleExpression
import cspom.util.IntInterval
import cspom.variable.IntExpression


/**
 * @author vion
 */
object OccurrenceDomains extends VariableCompiler('occurrence) {
  def compiler(c: CSPOMConstraint[_]): Map[CSPOMExpression[_], CSPOMExpression[_]] = {
    val CSPOMConstraint(r, _, Seq(const @ CSPOMConstant(value), CSPOMSeq(args)), _) = c
    val ir = IntExpression.coerce(r)
    val lb = args.count(_ == const)
    val ub = args.count {
      case s: SimpleExpression[_] => s.contains(value)
      case _ => throw new IllegalArgumentException("Occurrence constraint only supports SimpleExpressions")
    }

    Map(r -> reduceDomain(ir, IntInterval(lb, ub)))
  }
}