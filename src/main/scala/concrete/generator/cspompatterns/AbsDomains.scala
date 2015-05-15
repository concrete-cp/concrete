package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.compiler.VariableCompiler
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.CSPOM

object AbsDomains extends VariableCompiler('abs) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(r: SimpleExpression[_], _, Seq(i: SimpleExpression[_]), _) =>
      val ir = IntExpression.coerce(r)
      val ii = IntExpression.coerce(i)
      Map(
        r -> reduceDomain(ir, ii.abs),
        i -> reduceDomain(ii, ir ++ -ir))
  }
}

object ConstAbs extends ConstraintCompiler {
  type A = (SimpleExpression[_], SimpleExpression[_])
  override def matcher = {
    case (CSPOMConstraint(r: SimpleExpression[_], 'abs, Seq(i: SimpleExpression[_]), _), _) if r.isInstanceOf[CSPOMConstant[_]] || i.isInstanceOf[CSPOMConstant[_]] =>
      (r, i)
  }

  def selfPropagation = false
  
  def compile(c: CSPOMConstraint[_], cspom: CSPOM, d: A) = {
    val (r, i) = d
    val ir = IntExpression.coerce(r)
    val ii = IntExpression.coerce(i)

    require(!(ir & ii.abs).isEmpty, "Unconsistency detected for constraint " + c)
    require(!(ii & (ir ++ -ir)).isEmpty, "Unconsistency detected for constraint " + c)

    replaceCtr(c, Nil, cspom)
  }

}