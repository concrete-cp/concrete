package concrete

import scala.annotation.varargs
import scala.reflect.runtime.universe._

import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
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

  def linear(vars: Seq[SimpleExpression[Int]], coefs: Seq[Int], mode: String, constant: Int): CSPOMConstraint[Boolean] =
    CSPOMConstraint('sum)(coefs, vars, CSPOMConstant(constant)) withParam ("mode" -> mode)

  def pseudoBoolean(vars: Seq[SimpleExpression[Boolean]], coefs: Seq[Int], mode: String, constant: Int): CSPOMConstraint[Boolean] =
    CSPOMConstraint('pseudoboolean)(coefs, vars, CSPOMConstant(constant)) withParam ("mode" -> mode)

  def abs(variable: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.defineInt(r => CSPOMConstraint(r)('abs)(variable))
  }

  def sq(v: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.defineInt(r => CSPOMConstraint(r)('sq)(v))
  }

  def allDifferent(v: SimpleExpression[Int]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('alldifferent)(v: _*)
  }

  def gcc(cardinalities: Seq[(Int, Int, Int)], v: SimpleExpression[Int]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('gcc)(v: _*) withParam ("gcc" -> cardinalities)
  }

  def occurrence[A: TypeTag](value: SimpleExpression[A])(variables: SimpleExpression[A]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.defineInt(r => CSPOMConstraint(r)('occurrence)(value, seq2CSPOMSeq(variables)))
  }

  def atLeast[A: TypeTag](count: SimpleExpression[Int], value: SimpleExpression[A], variables: SimpleExpression[A]*)(implicit problem: CSPOM): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('atLeast)(count, value, seq2CSPOMSeq(variables))
  }

  def atMost[A: TypeTag](count: SimpleExpression[Int], value: SimpleExpression[A], variables: SimpleExpression[A]*)(implicit problem: CSPOM): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('atMost)(count, value, seq2CSPOMSeq(variables))
  }

  def clause(positive: SimpleExpression[Boolean]*)(negative: SimpleExpression[Boolean]*): CSPOMConstraint[Boolean] = {
    clause(positive, negative)
  }

  def clause(positive: CSPOMSeq[Boolean], negative: CSPOMSeq[Boolean]): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('clause)(positive, negative)
  }

  def and(vars: SimpleExpression[Boolean]*)(implicit problem: CSPOM): SimpleExpression[Boolean] = {
    val r = problem.defineBool(r => CSPOMDriver.clause(r)(vars: _*))
    for (v <- vars) {
      problem.postpone(CSPOMDriver.clause(v)(r))
    }
    r
  }

  def or(vars: SimpleExpression[Boolean]*)(implicit problem: CSPOM): SimpleExpression[Boolean] = {
    val r = problem.defineBool(r => CSPOMDriver.clause(vars: _*)(r))
    for (v <- vars) {
      problem.postpone(CSPOMDriver.clause(r)(v))
    }
    r
  }

  def nogood(vars: SimpleExpression[Boolean]*): CSPOMConstraint[Boolean] = {
    clause()(vars: _*)
  }

  implicit class CSPOMSeqOperations[+A](e: CSPOMSeq[A]) {
    def apply(idx: CSPOMVariable[Int])(implicit problem: CSPOM) =
      problem.defineFree(r => CSPOMConstraint(r)('element)(e, idx))

    def min(implicit problem: CSPOM) =
      problem.defineFree(r => CSPOMConstraint(r)('min)(e: _*))

    def max(implicit problem: CSPOM) =
      problem.defineFree(r => CSPOMConstraint(r)('max)(e: _*))

    def <=[B >: A](that: CSPOMSeq[B])(implicit problem: CSPOM) = {
      require(e.size == that.size)
      problem.defineBool(r => CSPOMConstraint(r)('lexleq)(e, that))
    }

    def contains[B >: A](v: SimpleExpression[B])(implicit problem: CSPOM) =
      problem.defineBool(r => CSPOMConstraint(r)('in)(v, e))
  }

  implicit class CSPOMIntExpressionOperations(e: SimpleExpression[Int]) {
    def >(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      other < e

    def >=(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      other <= e

    def <(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.defineBool(result => CSPOMConstraint(result)('sum)(Seq(1, -1), Seq(e, other), CSPOMConstant(0)) withParam
        ("mode" -> "lt"))

    def <=(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.defineBool(result => CSPOMConstraint(result)('sum)(Seq(1, -1), Seq(e, other), CSPOMConstant(0)) withParam
        ("mode" -> "le"))

    def +(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
      problem.defineInt(r =>
        CSPOMConstraint('sum)(Seq(1, 1, -1), Seq(e, other, r), CSPOMConstant(0)) withParam
          ("mode" -> "eq"))
    }

    def -(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
      problem.defineInt(r =>
        CSPOMConstraint('sum)(Seq(1, -1, -1), Seq(e, other, r), CSPOMConstant(0)) withParam
          ("mode" -> "eq"))
    }

    def *(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] =
      problem.defineInt(result => CSPOMConstraint(result)('mul)(e, other))

    def /(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] =
      problem.defineInt(result => CSPOMConstraint(result)('div)(e, other))

    def %(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] =
      problem.defineInt(result => CSPOMConstraint(result)('mod)(e, other))
  }

  implicit class CSPOMBoolExpressionOperations(e: SimpleExpression[Boolean]) {
    def |(other: SimpleExpression[Boolean])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      or(e, other)

    def &(other: SimpleExpression[Boolean])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      and(e, other)

    def unary_!(implicit problem: CSPOM): SimpleExpression[Boolean] = {
      problem.defineBool(r => CSPOMConstraint(r)('not)(e))
    }

    def ==>(other: SimpleExpression[Boolean])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.defineBool(r => CSPOMConstraint('clause)(Seq(other), Seq(e)))
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

  def seq(v: Array[CSPOMExpression[Int]]): CSPOMSeq[Int] = {
    seq2CSPOMSeq(v)
  }

  //def nameArray(v: Array[CSPOMExpression[Any]], name:String) = problem.nameExpression(e, n)
}

