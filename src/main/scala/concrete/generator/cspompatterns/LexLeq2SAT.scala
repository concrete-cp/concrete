package concrete.generator.cspompatterns

import concrete.CSPOMDriver.{CSPOMBoolExpressionOperations, CSPOMIntExpressionOperations, and, or}
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Functions}
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}

object LexLeq2SAT extends ConstraintCompilerNoData {

  def functions = Functions("lexleq")

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(x: CSPOMSeq[_], y: CSPOMSeq[_]), _) =>
      (x ++ y).forall {
        case BoolExpression(v) => true
        case v if BoolExpression.is01(v) => true
        case _ => false
      }
    case _ => false
  }

  def selfPropagation: Boolean = false

  def compile(constraint: cspom.CSPOMConstraint[_], problem: cspom.CSPOM): cspom.compiler.Delta = {
    val Seq(CSPOMSeq(x), CSPOMSeq(y)) = constraint.arguments

    ConstraintCompiler.removeCtr(constraint, problem)
  }

  def lexLeq(x: Seq[SimpleExpression[_]], y: Seq[SimpleExpression[_]])(implicit problem: CSPOM): SimpleExpression[Boolean] = {
    val n = x.size
    require(y.size == n)

    or(
      lt(x.head, y.head) +:
        and(
          (0 until n).map(i => x(i) === y(i)): _*) +:
        (0 until n - 1).map { i =>
          and(
            lt(x(i + 1), y(i + 1)) +:
              (0 until i).map(j => x(j) === y(j)): _*) //problem.isBool("and", conjunction)
        }: _*)

  }

  def lt(x: CSPOMExpression[_], y: CSPOMExpression[_])(implicit problem: CSPOM): SimpleExpression[Boolean] =
    (x, y) match {
      case (BoolExpression(xi), BoolExpression(yi)) => xi ==> yi // problem.isBool("clause", Seq(Seq(yi), Seq(xi)))
      case (IntExpression(xi), IntExpression(yi)) => xi < yi // problem.isBool("lt", Seq(xi, yi))
    }

}