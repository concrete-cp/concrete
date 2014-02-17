package concrete

import scala.annotation.varargs
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.BoolExpression
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntExpression
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression
import cspom.variable.IntConstant
import CSPOM._

object CSPOMDriver {

  def sum(variables: SimpleExpression*)(implicit problem: CSPOM): IntExpression = {
    sumProd(variables.map((1, _)): _*)
    //    val result = IntVariable.free()
    //    problem.ctr(new CSPOMConstraint(
    //      'sum,
    //      Seq[CSPOMExpression](result +: variables, 0),
    //      Map("mode" -> "SumEq", "coefficients" -> (-1 +: Seq.fill(variables.size)(1)))))
    //    result
  }

  def sumProd(coefVar: (Int, SimpleExpression)*)(implicit problem: CSPOM): IntExpression = {
    val (coefs, vars) = coefVar.unzip
    //problem.isInt('sum, vars, Map("coefficients" -> coefs))

    val result = IntVariable.free()
    problem.ctr(new CSPOMConstraint(
      'sum,
      Seq[CSPOMExpression](result +: vars, 0),
      Map("mode" -> "eq", "coefficients" -> (-1 +: coefs))))
    result
  }

  def abs(variable: IntExpression)(implicit problem: CSPOM): IntExpression = {
    problem.isInt('abs, Seq(variable))
  }

  def lexLeq(v0: Seq[CSPOMVariable], v1: Seq[CSPOMVariable])(implicit problem: CSPOM): CSPOMConstraint = {
    new CSPOMConstraint('lexleq, Seq(CSPOMSeq(v0: _*), CSPOMSeq(v1: _*)))
  }

  def sq(v: IntExpression)(implicit problem: CSPOM): IntExpression = {
    problem.isInt('sq, Seq(v))
  }

  def allDifferent(v: IntExpression*): CSPOMConstraint = {
    new CSPOMConstraint('allDifferent, v)
  }

  def gcc(cardinalities: Seq[(Int, Int, Int)], v: IntExpression*): CSPOMConstraint = {
    new CSPOMConstraint('gcc, v, Map("gcc" -> cardinalities))
  }

  def occurrence[A](constant: A with CSPOMConstant, variables: A with SimpleExpression*)(implicit problem: CSPOM): IntExpression = {
    problem.isInt('occurrence, variables, Map("occurrence" -> constant))
  }

  implicit class CSPOMExpressionOperations(e: CSPOMExpression) {
    def !==(other: CSPOMExpression)(implicit problem: CSPOM): BoolVariable = problem.isBool('ne, Seq(e, other))

    def ≠(other: CSPOMExpression)(implicit problem: CSPOM): BoolVariable = this !== other

    def ===(other: CSPOMExpression)(implicit problem: CSPOM): BoolVariable = problem.isBool('eq, Seq(e, other))
  }

  implicit class CSPOMIntExpressionOperations(e: IntExpression) {
    def >(other: IntExpression)(implicit problem: CSPOM) = problem.isBool('gt, Seq(e, other))

    def >=(other: IntExpression)(implicit problem: CSPOM) = problem.isBool('ge, Seq(e, other))

    def <(other: IntExpression)(implicit problem: CSPOM) = problem.isBool('gt, Seq(other, e))

    def <=(other: IntExpression)(implicit problem: CSPOM) = problem.isBool('ge, Seq(other, e))

    def +(other: IntExpression)(implicit problem: CSPOM) = problem.isInt('add, Seq(e, other))

    def -(other: IntExpression)(implicit problem: CSPOM) = problem.isInt('sub, Seq(e, other))

    def *(other: IntExpression)(implicit problem: CSPOM) = problem.isInt('mul, Seq(e, other))

    def /(other: IntExpression)(implicit problem: CSPOM) = problem.isInt('div, Seq(e, other))
  }

  implicit class CSPOMBoolExpressionOperations(e: BoolExpression) {
    def |(other: BoolExpression)(implicit problem: CSPOM) = problem.isBool('or, Seq(e, other))

    def &(other: BoolExpression)(implicit problem: CSPOM) = problem.isBool('and, Seq(e, other))

    def unary_!(implicit problem: CSPOM): BoolVariable = {
      problem.isBool('not, Seq(e))
    }

    def ==>(other: BoolExpression)(implicit problem: CSPOM) =
      problem.isBool('or, Seq(e, other), Map("revsign" -> Array(true, false)))
  }
}

final object JCSPOMDriver {
  import CSPOMDriver._

  def ne[A <: SimpleExpression, B <: SimpleExpression](e1: A, e2: B) = e1 !== e2

  @varargs
  def allDifferent(v: IntVariable*) = CSPOMDriver.allDifferent(v: _*)

  def abs(v: IntVariable) = CSPOMDriver.abs(v)

  def lt(v1: IntExpression, v2: IntExpression) = v1 < v2

  def less(v1: IntExpression, v2: IntExpression) = v1 - v2
}
