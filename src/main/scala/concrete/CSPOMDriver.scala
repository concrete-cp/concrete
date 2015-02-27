package concrete

import scala.annotation.varargs
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression
import CSPOM._
import cspom.variable.BoolVariable

object CSPOMDriver {

  def sum(variables: SimpleExpression[Int]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    sumProd(variables.map((1, _)): _*)(problem)
  }

  def sumProd(coefVar: (Int, SimpleExpression[Int])*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    val (coefs, vars) = coefVar.unzip

    val result = IntVariable.free()
    problem.ctr(CSPOMConstraint(
      'sum,
      Seq(CSPOMSeq[Int](result +: vars: _*), CSPOMConstant(0)),
      Map("mode" -> "eq", "coefficients" -> (-1 +: coefs))))
    result
  }

  def abs(variable: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.isInt('abs, Seq(variable))
  }

  def sq(v: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.isInt('sq, Seq(v))
  }

  def allDifferent(v: SimpleExpression[Int]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('allDifferent, v)
  }

  def gcc(cardinalities: Seq[(Int, Int, Int)], v: SimpleExpression[Int]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('gcc, v, Map("gcc" -> cardinalities))
  }

  def occurrence[A](value: SimpleExpression[A], variables: SimpleExpression[A]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.isInt('occurrence, Seq(value, variables))
  }

  def clause(positive: Seq[SimpleExpression[Boolean]], negative: Seq[SimpleExpression[Boolean]]): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('clause, Seq(positive, negative))
  }

  implicit class CSPOMSeqOperations[+A](e: CSPOMSeq[A]) {
    def apply(idx: CSPOMVariable[Int])(implicit problem: CSPOM) = problem.is('element, Seq(e, idx))

    def min(implicit problem: CSPOM) = problem.is('min, Seq(e))

    def max(implicit problem: CSPOM) = problem.is('max, Seq(e))

    def <=[B >: A](that: CSPOMSeq[B])(implicit problem: CSPOM) = {
      require(e.size == that.size)
      problem.isBool('lexleq, Seq(e, that))
    }

    def contains[B >: A](v: SimpleExpression[B])(implicit problem: CSPOM) = problem.isBool('in, Seq(v, e))
  }

  implicit class MoreSeqOperations[+A](e: Seq[CSPOMExpression[A]]) extends CSPOMSeqOperations(seq2CSPOMSeq(e))

  implicit class CSPOMIntExpressionOperations(e: SimpleExpression[Int]) {
    def >(other: SimpleExpression[Int])(implicit problem: CSPOM) = problem.isBool('gt, Seq(e, other))

    def >=(other: SimpleExpression[Int])(implicit problem: CSPOM) = problem.isBool('ge, Seq(e, other))

    def <(other: SimpleExpression[Int])(implicit problem: CSPOM) = problem.isBool('gt, Seq(other, e))

    def <=(other: SimpleExpression[Int])(implicit problem: CSPOM) = problem.isBool('ge, Seq(other, e))

    def +(other: SimpleExpression[Int])(implicit problem: CSPOM) = problem.isInt('add, Seq(e, other))

    def -(other: SimpleExpression[Int])(implicit problem: CSPOM) = problem.isInt('sub, Seq(e, other))

    def *(other: SimpleExpression[Int])(implicit problem: CSPOM) = problem.isInt('mul, Seq(e, other))

    def /(other: SimpleExpression[Int])(implicit problem: CSPOM) = problem.isInt('div, Seq(e, other))
  }

  implicit class CSPOMBoolExpressionOperations(e: SimpleExpression[Boolean]) {
    def |(other: SimpleExpression[Boolean])(implicit problem: CSPOM) = problem.isBool('clause, Seq(Seq(e, other), CSPOMSeq.empty))

    def &(other: SimpleExpression[Boolean])(implicit problem: CSPOM) = problem.isBool('and, Seq(e, other))

    def unary_!(implicit problem: CSPOM): BoolVariable = {
      problem.isBool('not, Seq(e))
    }

    def ==>(other: SimpleExpression[Boolean])(implicit problem: CSPOM) =
      problem.isBool('clause, Seq(Seq(other), Seq(e)))
  }
}

final class JCSPOMDriver extends CSPOM {
  import CSPOMDriver._

  implicit def problem: CSPOM = this

  def lt(v1: SimpleExpression[Int], v2: SimpleExpression[Int]) = v1 < v2

  def less(v1: SimpleExpression[Int], v2: SimpleExpression[Int]) = v1 - v2

  def neq(e1: CSPOMExpression[Any], e2: CSPOMExpression[Any]) = e1 !== e2

  @varargs
  def allDifferent(v: IntVariable*) = CSPOMDriver.allDifferent(v: _*)

  def abs(v: SimpleExpression[Int]) = CSPOMDriver.abs(v)

  def seq[A](v: Array[CSPOMExpression[A]]): CSPOMSeq[A] = CSPOMSeq(v: _*)

  //def nameArray(v: Array[CSPOMExpression[Any]], name:String) = problem.nameExpression(e, n)
}

