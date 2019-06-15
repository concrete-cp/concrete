package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler

import cspom.variable.BoolExpression
import cspom.variable.SimpleExpression

object ClauseDomains extends VariableCompiler("clause") {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r, _, Seq(SimpleExpression.simpleCSPOMSeq(pos), SimpleExpression.simpleCSPOMSeq(neg)), _) =>

      Seq(r -> BoolExpression.coerce(r)) ++
        pos.map(p => p -> BoolExpression.coerce(p)) ++
        neg.map(n => n -> BoolExpression.coerce(n))

  }
}

