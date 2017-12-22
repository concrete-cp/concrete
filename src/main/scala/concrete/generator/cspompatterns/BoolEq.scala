package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import concrete.constraint.linear.SumEQ
import concrete.generator.SumGenerator
import cspom.CSPOM._
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta}
import cspom.variable.{BoolExpression, CSPOMSeq, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}

/**
  * Reified boolean equality:
  *
  * r <-> (a <-> b)
  *
  * is
  *
  * r -> (a \/ -b),  r -> (-a \/  b),
  * -r -> (a \/  b), -r -> (-a \/ -b)
  *
  */
object BoolEq extends ConstraintCompiler {

  type A = Seq[SimpleExpression[_]]

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(
    r, 'eq,
    Seq(BoolExpression.bool01(a), BoolExpression.bool01(b)), _) =>
      Some(Seq(a, b))
    case _ => None
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {

    val Seq(a, b) = data.map(BoolExpression.coerce(_))

    val nr = BoolExpression.coerce(c.result)

    replace(c.result, nr, problem) ++
      replaceCtr(c, Seq(
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

      if (vars.forall(BoolExpression.is01)) {
        //val (boolVars, bool2Int) = intOrBoolToBool(vars)

        PartialFunction.condOpt((coefs, constant, mode)) {
          case (Seq(1), 1, SumEQ) => Seq(CSPOMConstraint(c.result)('clause)(vars, CSPOMSeq()))
          case (Seq(1), 0, SumEQ) => Seq(CSPOMConstraint(c.result)('clause)(CSPOMSeq(), vars))
          case (Seq(1, 1), 1, SumEQ) => Seq(CSPOMConstraint(c.result)('xor)(vars: _*))
        }


      } else {
        None
      }
    } else {
      None
    }
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {

    replaceCtr(fc, data, problem)

  }
}

object BoolProd extends ConstraintCompilerNoData {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'mul && c.arguments.forall(BoolExpression.is01)
  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM): Delta = {

    val c = CSPOMConstraint(fc.result)('and)(fc.arguments: _*)

    replaceCtr(fc, c, problem)

  }
}