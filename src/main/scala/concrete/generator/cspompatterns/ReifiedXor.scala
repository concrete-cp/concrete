package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData}
import cspom.variable.SimpleExpression
import CSPOM._

/**
 * Reified XOR is converted to CNF :
 *
 * a = b (+) c
 *
 * <=>
 *
 * a = (b \/ c) /\ (-b \/ -c)
 */
object ReifiedXor extends ConstraintCompilerNoData {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'xor && !c.result.isTrue && c.arguments.length == 2
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM) = {

    val Seq(a: SimpleExpression[_], b: SimpleExpression[_]) = fc.arguments
    val res = fc.result

    val alt1 = CSPOMConstraint(new BoolVariable())('clause)(Seq(a, b), Seq[SimpleExpression[Boolean]]())
    val alt2 = CSPOMConstraint(new BoolVariable())('clause)(Seq[SimpleExpression[Boolean]](), Seq(a, b))
    val conj = CSPOMConstraint(res)('and)(alt1.result, alt2.result)

    ConstraintCompiler.replaceCtr(fc, Seq(alt1, alt2, conj), problem)

  }

  def selfPropagation = false

}
