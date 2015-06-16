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
import scala.reflect.runtime.universe._

object CSPOMDriver {

  def sum(variables: SimpleExpression[Int]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    sumProd(variables.map((1, _)): _*)(problem)
  }

  def sumProd(coefVar: (Int, SimpleExpression[Int])*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    val result = IntVariable.free()
    problem.postpone(linear((-1, result) +: coefVar, "eq", 0))
    result
  }

  def linear(scalar: Seq[(Int, SimpleExpression[Int])], mode: String, constant: Int): CSPOMConstraint[Boolean] = {
    val (coefs, vars) = scalar.unzip
    linear(vars, coefs, mode, constant)
  }

  def linear(vars: Seq[SimpleExpression[Int]], coefs: Seq[Int], mode: String, constant: Int): CSPOMConstraint[Boolean] = {
    CSPOMConstraint(
      'sum,
      Seq(CSPOMSeq(vars: _*), CSPOMConstant(constant)),
      Map("mode" -> "eq", "coefficients" -> coefs))
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

  def occurrence[A: TypeTag](value: SimpleExpression[A])(variables: SimpleExpression[A]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.isInt('occurrence, Seq(value, seq2CSPOMSeq(variables)))
  }

  //  def clause(positive: Seq[SimpleExpression[Boolean]], negative: Seq[SimpleExpression[Boolean]]): CSPOMConstraint[Boolean] = {
  //    CSPOMConstraint('clause, Seq(positive, negative))
  //  }

  def clause(positive: SimpleExpression[Boolean]*)(negative: SimpleExpression[Boolean]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('clause, Seq(positive, negative))
  }

  def and(vars: SimpleExpression[Boolean]*)(implicit problem: CSPOM): SimpleExpression[Boolean] = {
    problem.isBool('and, vars)
  }

  def or(vars: SimpleExpression[Boolean]*)(implicit problem: CSPOM): SimpleExpression[Boolean] = {
    problem.isBool('clause, Seq(vars, CSPOMSeq()))
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

  //implicit class MoreSeqOperations[+A: TypeTag](e: Seq[CSPOMExpression[A]]) extends CSPOMSeqOperations(seq2CSPOMSeq(e))

  implicit class CSPOMIntExpressionOperations(e: SimpleExpression[Int]) {
    def >(other: SimpleExpression[Int])(implicit problem: CSPOM) = other < e

    def >=(other: SimpleExpression[Int])(implicit problem: CSPOM) = other <= e

    def <(other: SimpleExpression[Int])(implicit problem: CSPOM) =
      problem.isBool('sum, Seq(CSPOMSeq(e, other), CSPOMConstant(0)),
        Map("coefficients" -> Seq(1, -1), "mode" -> "lt"))

    def <=(other: SimpleExpression[Int])(implicit problem: CSPOM) =
      problem.isBool('sum, Seq(CSPOMSeq(e, other), CSPOMConstant(0)),
        Map("coefficients" -> Seq(1, -1), "mode" -> "le"))

    def +(other: SimpleExpression[Int])(implicit problem: CSPOM) = {
      val r = IntVariable.free()
      problem.postpone(CSPOMConstraint('sum, Seq(CSPOMSeq(e, other, r), CSPOMConstant(0)),
        Map("coefficients" -> Seq(1, 1, -1), "mode" -> "eq")))
      r
    }

    def -(other: SimpleExpression[Int])(implicit problem: CSPOM) = {
      val r = IntVariable.free()
      problem.postpone(CSPOMConstraint('sum, Seq(CSPOMSeq(e, other, r), CSPOMConstant(0)),
        Map("coefficients" -> Seq(1, -1, -1), "mode" -> "eq")))
      r
    }

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

  def seq[A: TypeTag](v: Array[CSPOMExpression[A]]): CSPOMSeq[A] = CSPOMSeq(v: _*)

  //def nameArray(v: Array[CSPOMExpression[Any]], name:String) = problem.nameExpression(e, n)
}

