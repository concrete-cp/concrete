package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.VariableCompiler
import cspom.variable.IntExpression.implicits.{arithmetics, ranges}
import cspom.variable.{IntExpression, SimpleExpression}

object AbsDomains extends VariableCompiler('abs) {

  def compiler(c: CSPOMConstraint[_]) = {

    val CSPOMConstraint(r: SimpleExpression[_], _, Seq(i: SimpleExpression[_]), _) = c
    val ir = IntExpression.coerce(r)
    val ii = IntExpression.coerce(i)

    val res = Seq(
      r -> reduceDomain(ir, ii.abs),
      i -> reduceDomain(ii, ir ++ -ir))

    res
  }
}

