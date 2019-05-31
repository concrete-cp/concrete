package concrete

import cspom.CSPOM._
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint, CSPOMGoal}

import scala.annotation.varargs
import scala.reflect.runtime.universe._

object SumBuilder {
  def apply(v: SimpleExpression[Int]) = new SumBuilder(Seq(v), Seq(1))
  def apply(coef: Int, v: SimpleExpression[Int]) = new SumBuilder(Seq(v), Seq(coef))
}

class SumBuilder(val variables: Seq[SimpleExpression[Int]], val coefs: Seq[Int]) {
  def <(i: Int): CSPOMConstraint[Boolean] = CSPOMDriver.linear(variables, coefs, "lt", i)

  def ===(i: Int): CSPOMConstraint[Boolean] = CSPOMDriver.linear(variables, coefs, "eq", i)

  def <=(i: Int): CSPOMConstraint[Boolean] = CSPOMDriver.linear(variables, coefs, "le", i)

  def >=(i: Int): CSPOMConstraint[Boolean] = CSPOMDriver.linear(variables, coefs.map(-_), "le", i)

  def >(i: Int): CSPOMConstraint[Boolean] = CSPOMDriver.linear(variables, coefs.map(-_), "lt", i)

  def !==(i: Int): CSPOMConstraint[Boolean] = CSPOMDriver.linear(variables, coefs, "ne", i)

  def +(sb: SumBuilder) = new SumBuilder(variables ++ sb.variables, coefs ++ sb.coefs)

  def +(se: SimpleExpression[Int]) = new SumBuilder(variables :+ se, coefs :+ 1)
}

object CSPOMDriver {

  def sum(variables: SimpleExpression[Int]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    sumProd(variables.map((1, _)): _*)
  }

  def sumProd(coefVar: (Int, SimpleExpression[Int])*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    val (coefs, vars) = coefVar.unzip
    problem.defineInt(result => linear(result +: vars, -1 +: coefs, "eq", 0))
  }

  def linear[A: TypeTag](vars: Seq[SimpleExpression[A]], coefs: Seq[Int], mode: String, constant: Int): CSPOMConstraint[Boolean] =
    CSPOMConstraint('sum)(coefs, vars, CSPOMConstant(constant)) withParam ("mode" -> mode)

  def linearReif(mode: String, constant: Int, scalar: (Int, SimpleExpression[Int])*)(implicit problem: CSPOM): SimpleExpression[Boolean] = {
    val (coefs, vars) = scalar.unzip
    linearReif(vars, coefs, mode, constant)
  }

  def linearReif(vars: Seq[SimpleExpression[Int]], coefs: Seq[Int], mode: String, constant: Int)
                (implicit problem: CSPOM): SimpleExpression[Boolean] =
    problem.defineBool(result =>
      CSPOMConstraint(result)('sum)(coefs, vars, CSPOMConstant(constant)) withParam ("mode" -> mode))

  def pseudoBoolean(vars: CSPOMSeq[_], coefs: CSPOMSeq[_], mode: String, constant: Int): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('pseudoboolean)(coefs, vars, CSPOMConstant(constant)) withParam ("mode" -> mode)
  }

  def pseudoBoolean(vars: Seq[SimpleExpression[Boolean]], coefs: Seq[Int], mode: String, constant: Int): CSPOMConstraint[Boolean] = {
    pseudoBoolean(seq2CSPOMSeq(vars), seq2CSPOMSeq(coefs), mode, constant)
  }


  def abs(variable: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.defineInt(r => CSPOMConstraint(r)('abs)(variable))
  }

  def sq(v: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.defineInt(r => CSPOMConstraint(r)('sq)(v))
  }

  def allDifferent(v: SimpleExpression[Int]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('alldifferent)(v: _*)
  }

  def gccExact(v: Seq[SimpleExpression[Int]], closed: Boolean, values: Seq[Int], occurrences: Seq[Int]): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('gccExact)(v, closed, values, occurrences)
  }

  def occurrence[A: TypeTag](value: SimpleExpression[A])(variables: SimpleExpression[A]*)(implicit problem: CSPOM): SimpleExpression[Int] = {
    problem.defineInt(r => CSPOMConstraint(r)('occurrence)(value, seq2CSPOMSeq(variables)))
  }

  def atLeast[A: TypeTag](count: SimpleExpression[Int], value: SimpleExpression[A], variables: SimpleExpression[A]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('atLeast)(count, value, seq2CSPOMSeq(variables))
  }

  def atMost[A: TypeTag](count: SimpleExpression[Int], value: SimpleExpression[A], variables: SimpleExpression[A]*): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('atMost)(count, value, seq2CSPOMSeq(variables))
  }

  def and(vars: SimpleExpression[Boolean]*)(implicit problem: CSPOM): SimpleExpression[Boolean] = {
    val r = problem.defineBool(r => CSPOMDriver.clause(r)(vars: _*))
    for (v <- vars) {
      problem.postpone(CSPOMDriver.clause(v)(r))
    }
    r
  }

  def clause(positive: CSPOMExpression[_]*)(negative: CSPOMExpression[_]*): CSPOMConstraint[Boolean] = {
    clause(CSPOMSeq(positive: _*), CSPOMSeq(negative: _*))
  }

  def clause(positive: CSPOMSeq[_], negative: CSPOMSeq[_]): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('clause)(positive, negative)
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

  def cumulative(starts: CSPOMSeq[Int], durations: CSPOMSeq[Int], resources: CSPOMSeq[Int], bound: SimpleExpression[Int]): CSPOMConstraint[Boolean] = {
    CSPOMConstraint('cumulative)(starts, durations, resources, bound)
  }

  implicit class CSPOMSeqOperations[+A](e: CSPOMSeq[A]) {
    def apply(idx: CSPOMVariable[Int])(implicit problem: CSPOM): SimpleExpression[_] =
      problem.defineFree(r => CSPOMConstraint(r)('element)(e, idx))

    def cmin(implicit problem: CSPOM): SimpleExpression[A] =
      problem.define(new FreeVariable().asInstanceOf[SimpleExpression[A]])(
        r => CSPOMConstraint(r)('min)(e: _*))

    def cmax(implicit problem: CSPOM): SimpleExpression[A] =
      problem.define(new FreeVariable().asInstanceOf[SimpleExpression[A]])(
        r => CSPOMConstraint(r)('max)(e: _*))

    def <=[B >: A](that: CSPOMSeq[B])(implicit problem: CSPOM): SimpleExpression[Boolean] = {
      require(e.size == that.size)
      problem.defineBool(r => CSPOMConstraint(r)('lexleq)(e, that))
    }

    def contains[B >: A](v: SimpleExpression[B])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.defineBool(r => CSPOMConstraint(r)('in)(v, e))
  }

  implicit class CSPOMIntExpressionOperations(e: SimpleExpression[Int]) {
    def >(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      other < e

    def <(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.defineBool(result => CSPOMConstraint(result)('sum)(Seq(1, -1), Seq(e, other), CSPOMConstant(0)) withParam
        ("mode" -> "lt"))

    def >=(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      other <= e

    def <=(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Boolean] =
      problem.defineBool(result => CSPOMConstraint(result)('sum)(Seq(1, -1), Seq(e, other), CSPOMConstant(0)) withParam
        ("mode" -> "le"))

    def +(other: SimpleExpression[Int])(implicit problem: CSPOM): SimpleExpression[Int] = {
      problem.defineInt(r =>
        linear(Seq(e, other, r), Seq(1, 1, -1), "eq", 0))
//        CSPOMConstraint('sum)(Seq(1, 1, -1), Seq(e, other, r), CSPOMConstant(0)) withParam
//          ("mode" -> "eq"))
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

    def *:(i: Int) = new SumBuilder(Seq(e), Seq(i))
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

  def minimize(e: CSPOMExpression[Int]): Unit = setGoal(CSPOMGoal.Minimize(e))
  def maximize(e: CSPOMExpression[Int]): Unit = setGoal(CSPOMGoal.Maximize(e))

  //def nameArray(v: Array[CSPOMExpression[Any]], name:String) = problem.nameExpression(e, n)
}

