package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import Generator._
import concrete.constraint.semantic.Inverse
import concrete.constraint.semantic.InvExpression
import scala.collection.mutable.ArrayBuffer

final object InverseGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(x, y) = constraint.arguments.map(cspom2concreteIndexedSeq)

    var i = 0
    val allVars = new ArrayBuffer[Variable]()

    val xExpr = addVars(x, allVars)
    val yExpr = addVars(y, allVars)
    assignConstants(xExpr, yExpr)
    assignConstants(yExpr, xExpr)
    Seq(new Inverse(xExpr, yExpr, allVars.toArray))
  }

  def assignConstants(x: Seq[InvExpression], y: Seq[InvExpression]) {
    for (i <- x.indices) {
      x(i) match {
        case concrete.constraint.semantic.Const(c) => y(c).assign(i)
        case _ =>
      }
    }
  }

  def addVars(vars: Seq[(Int, C2Conc)], allVars: ArrayBuffer[Variable]): Array[InvExpression] = {
    val expr = new Array[InvExpression](vars.map(_._1).max + 1)
    for ((j, vx) <- vars) {
      expr(j) = vx match {
        case Var(v) => {
          val iv = concrete.constraint.semantic.Var(v, allVars.length)
          allVars += v
          iv
        }
        case Const(c) => concrete.constraint.semantic.Const(c)
        case _ => throw new AssertionError()
      }
    }
    expr
  }

}
