package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta}
import cspom.variable.IntExpression


object DiffNWCumulative extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = constraint.function == 'diffn_cumulative

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val Seq(IntExpression.simpleSeq(x), IntExpression.simpleSeq(y), IntExpression.simpleSeq(dx), IntExpression.simpleSeq(dy)) = constraint.arguments

    implicit def cspom = problem

    val minX = CSPOMSeqOperations(x).cmin
    val maxX = CSPOMSeqOperations((x, dx).zipped.map(_ + _)).cmax

    val minY = CSPOMSeqOperations(y).cmin
    val maxY = CSPOMSeqOperations((y, dy).zipped.map(_ + _)).cmax

    val c1 = cumulative(x, dx, dy, maxY - minY)
    val c2 = cumulative(y, dy, dx, maxX - minX)
    val d = CSPOMConstraint('diffn)(x, y, dx, dy)

    ConstraintCompiler.replaceCtr(constraint, Seq(c1, c2, d), problem)

  }

}