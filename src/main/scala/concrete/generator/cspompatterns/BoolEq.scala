package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.BoolExpression
import cspom.variable.SimpleExpression

import concrete.constraint.linear.SumEQ
import concrete.generator.SumGenerator
import cspom.variable.CSPOMSeq
import CSPOM._

/**
 * Reified boolean equality:
 *
 * r <-> (a <-> b)
 *
 * is
 *
 *  r -> (a \/ -b),  r -> (-a \/  b),
 * -r -> (a \/  b), -r -> (-a \/ -b)
 *
 */
object BoolEq extends ConstraintCompiler {

  type A = (SimpleExpression[_], SimpleExpression[_])

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(
      r, 'eq,
      Seq(BoolExpression.bool01(a), BoolExpression.bool01(b)), _) =>
      Some((a, b))
    case _ => None
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (ia, ib) = data

    val nr = BoolExpression.coerce(c.result)

    val (Seq(a, b), bool2int) = intOrBoolToBool(Seq(ia, ib))

    replace(c.result, nr, problem) ++
      replaceCtr(c, bool2int ++: Seq(
        CSPOMDriver.clause(b)(nr, a),
        CSPOMDriver.clause(a)(nr, b),
        CSPOMDriver.clause(nr, a, b)(),
        CSPOMDriver.clause(nr)(a, b)), problem)

  }

  def selfPropagation = true

}

object BoolSum extends ConstraintCompiler {
  type A = Seq[CSPOMConstraint[_]]

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = {
    if (c.function == 'sum) {

      val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(c)
      // val cond = vars.size == 1 && coefs.forall(c => c == 1) && (constant == 1 || constant == 0)

      if (vars.forall(BoolExpression.bool01.unapply(_).isDefined)) {
        val (boolVars, bool2Int) = intOrBoolToBool(vars)

        PartialFunction.condOpt((coefs, constant, mode)) {
          case (Seq(1), 1, SumEQ) => Seq(CSPOMConstraint(c.result)('clause)(boolVars, CSPOMSeq()))
          case (Seq(1), 0, SumEQ) => Seq(CSPOMConstraint(c.result)('clause)(CSPOMSeq(), boolVars))
          case (Seq(1, 1), 1, SumEQ) => Seq(CSPOMConstraint(c.result)('xor)(boolVars: _*))
        }
          .map(_ ++: bool2Int)

      } else {
        None
      }
    } else { None }
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    replaceCtr(fc, data, problem)

  }
}