package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import concrete.Variable
import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.BoolExpression
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.SimpleExpression
import cspom.variable.IntExpression

object LexLeq2SAT extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(CSPOMConstant(true), 'lexleq, Seq(x: CSPOMSeq[_], y: CSPOMSeq[_]), _) =>
      (x ++ y).forall {
        case BoolExpression(v)           => true
        case v if BoolExpression.is01(v) => true
        case _                           => false
      }
    case _ => false
  }

  def selfPropagation: Boolean = false

  def compile(constraint: cspom.CSPOMConstraint[_], problem: cspom.CSPOM): cspom.compiler.Delta = {
    val Seq(CSPOMSeq(x), CSPOMSeq(y)) = constraint.arguments

    removeCtr(constraint, problem)
  }

  def lexLeq(x: Seq[SimpleExpression[_]], y: Seq[SimpleExpression[_]])(implicit problem: CSPOM) = {
    val n = x.size
    require(y.size == n)

    or(
      lt(x(0), y(0)) +:
        and(
          (0 until n).map(i => x(i) === y(i)): _*) +:
          (0 until n - 1).map { i =>
            and(
              lt(x(i + 1), y(i + 1)) +:
                (0 until i).map(j => x(j) === y(j)): _*) //problem.isBool('and, conjunction)
          }: _*)

  }

  def lt(x: CSPOMExpression[_], y: CSPOMExpression[_])(implicit problem: CSPOM) = (x, y) match {
    case (BoolExpression(xi), BoolExpression(yi)) => xi ==> yi // problem.isBool('clause, Seq(Seq(yi), Seq(xi)))
    case (IntExpression(xi), IntExpression(yi))   => xi < yi // problem.isBool('lt, Seq(xi, yi))
  }

}