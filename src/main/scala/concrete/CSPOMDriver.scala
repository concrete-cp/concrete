package concrete

import cspom.variable.IntExpression
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import cspom.variable.CSPOMExpression
import cspom.variable.BoolExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable
import cspom.variable.CSPOMTrue
import cspom.CSPOMConstraint

object CSPOMDriver {
  def sum(variables: CSPOMVariable*)(implicit problem: CSPOM): IntExpression = {
    problem.isInt('sum, variables)
  }

  def sumProd(coefVar: (Int, CSPOMVariable)*)(implicit problem: CSPOM): IntExpression = {
    val (coefs, vars) = coefVar.unzip
    problem.isInt('sum, vars, Map("coefficients" -> coefs))
  }

  def abs(variable: IntExpression)(implicit problem: CSPOM): IntExpression = {
    problem.isInt('abs, Seq(variable))
  }

  def lexLeq(v0: Seq[IntVariable], v1: Seq[IntVariable])(implicit problem: CSPOM): CSPOMConstraint = {
    problem.ctr('lexleq, Seq(CSPOMSeq(v0: _*), CSPOMSeq(v1: _*)))
  }

  def sq(v: IntExpression)(implicit problem: CSPOM): IntExpression = {
    problem.isInt('sq, Seq(v))
  }

  def allDifferent(v: IntExpression*)(implicit problem: CSPOM): CSPOMConstraint = {
    problem.ctr('allDifferent, v)
  }

  def gcc(cardinalities: Seq[(Int, Int, Int)], v: IntExpression*)(implicit problem: CSPOM): CSPOMConstraint = {
    problem.ctr('gcc, v, Map("gcc" -> cardinalities))
  }

  def occurrence(value: Int, v: IntExpression*)(implicit problem: CSPOM): IntExpression = {
    problem.isInt('occurrence, v, Map("occurrence" -> value))
  }

  implicit class CSPOMExpressionOperations(e: CSPOMExpression) {
    def !==(other: CSPOMExpression)(implicit problem: CSPOM) = problem.isBool('ne, Seq(e, other))

    def ≠(other: CSPOMExpression)(implicit problem: CSPOM) = !==(other)

    def ===(other: CSPOMExpression)(implicit problem: CSPOM) = problem.isBool('eq, Seq(e, other))
  }

  implicit class CSPOMIntExpressionOperations(e: IntExpression) {
    def >(other: IntExpression)(implicit problem: CSPOM) = problem.isBool('gt, Seq(e, other))

    def >=(other: IntExpression)(implicit problem: CSPOM) = problem.isBool('ge, Seq(e, other))

    def <(other: IntExpression)(implicit problem: CSPOM) = problem.isBool('lt, Seq(e, other))

    def <=(other: IntExpression)(implicit problem: CSPOM) = problem.isBool('le, Seq(e, other))

    def +(other: IntExpression)(implicit problem: CSPOM) = problem.isInt('add, Seq(e, other))

    def -(other: IntExpression)(implicit problem: CSPOM) = problem.isInt('sub, Seq(e, other))

    def *(other: IntExpression)(implicit problem: CSPOM) = problem.isInt('mul, Seq(e, other))

    def /(other: IntExpression)(implicit problem: CSPOM) = problem.isInt('div, Seq(e, other))
  }

  implicit class CSPOMBoolExpressionOperations(e: BoolExpression) {
    def |(other: BoolExpression)(implicit problem: CSPOM) = problem.isBool('or, Seq(e, other))

    def &(other: BoolExpression)(implicit problem: CSPOM) = problem.isBool('and, Seq(e, other))
  }
}