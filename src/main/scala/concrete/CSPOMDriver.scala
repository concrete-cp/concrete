package concrete

import scala.annotation.varargs
import scala.reflect.runtime.universe._

import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression

object CSPOMDriver {

  def sum(variables: SimpleExpression[Int]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    sumProd(variables.map((1, _)): _*)
  }

  def sumProd(coefVar: (Int, SimpleExpression[Int])*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.defineInt(result => linear((-1, result) +: coefVar, "eq", 0))
  }

  def linear(scalar: Seq[(Int, SimpleExpression[Int])], mode: String, constant: Int): CSPOMConstraint[Boolean] = {
    val (coefs, vars) = scalar.unzip
    linear(vars, coefs, mode, constant)
  }

  def linear(vars: Seq[SimpleExpression[Int]], coefs: Seq[Int], mode: String, constant: Int): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('sum)(CSPOMSeq(vars: _*), CSPOMConstant(constant)) withParam
      ("mode" -> "eq", "coefficients" -> coefs)
  }

  def abs(variable: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.isInt('abs, Seq(variable))
  }

  def sq(v: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.isInt('sq, Seq(v))
  }

  def allDifferent(v: SimpleExpression[Int]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('allDifferent)(v: _*)
  }

  def gcc(cardinalities: Seq[(Int, Int, Int)], v: SimpleExpression[Int]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('gcc)(v: _*) withParam ("gcc" -> cardinalities)
  }

  def occurrence[A: TypeTag](value: SimpleExpression[A])(variables: SimpleExpression[A]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.isInt('occurrence, Seq(value, seq2CSPOMSeq(variables)))
  }

  def clause(positive: SimpleExpression[Boolean]*)(negative: SimpleExpression[Boolean]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('clause)(positive, negative)
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

  implicit class CSPOMIntExpressionOperations(e: SimpleExpression[Int]) {
    def >(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      other < e

    def >=(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      other <= e

    def <(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.defineBool(result => CSPOMConstraint(result)('sum)(CSPOMSeq(e, other), CSPOMConstant(0)) withParam
        ("coefficients" -> Seq(1, -1), "mode" -> "lt"))

    def <=(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.defineBool(result => CSPOMConstraint(result)('sum)(CSPOMSeq(e, other), CSPOMConstant(0)) withParam
        ("coefficients" -> Seq(1, -1), "mode" -> "le"))

    def +(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
      problem.defineInt(r =>
        CSPOMConstraint('sum)(CSPOMSeq(e, other, r), CSPOMConstant(0)) withParam
          ("coefficients" -> Seq(1, 1, -1), "mode" -> "eq"))
    }

    def -(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
      problem.defineInt(r =>
        CSPOMConstraint('sum)(CSPOMSeq(e, other, r), CSPOMConstant(0)) withParam
          ("coefficients" -> Seq(1, -1, -1), "mode" -> "eq"))
    }

    def *(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] =
      problem.defineInt(result => CSPOMConstraint(result)('mul)(e, other))

    def /(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] =
      problem.defineInt(result => CSPOMConstraint(result)('div)(e, other))
  }

  implicit class CSPOMBoolExpressionOperations(e: SimpleExpression[Boolean]) {
    def |(other: SimpleExpression[Boolean])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.isBool('clause, Seq(Seq(e, other), CSPOMSeq.empty))

    def &(other: SimpleExpression[Boolean])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.isBool('and, Seq(e, other))

    def unary_!(implicit problem: CSPOM): SimpleExpression[Boolean] = {
      problem.isBool('not, Seq(e))
    }

    def ==>(other: SimpleExpression[Boolean])(implicit problem: CSPOM): SimpleExpression[Boolean] =
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

