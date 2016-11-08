package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression

object DiffNWCumulative extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = constraint.function == 'diffn_cumulative

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val Seq(IntExpression.simpleSeq(x), IntExpression.simpleSeq(y), IntExpression.simpleSeq(dx), IntExpression.simpleSeq(dy)) = constraint.arguments

    implicit def cspom = problem

    val minX = CSPOMSeqOperations(x).min
    val maxX = CSPOMSeqOperations((x, dx).zipped.map(_ + _)).max

    val minY = CSPOMSeqOperations(y).min
    val maxY = CSPOMSeqOperations((y, dy).zipped.map(_ + _)).max

    val c1 = cumulative(x, dx, dy, maxY - minY)
    val c2 = cumulative(y, dy, dx, maxX - minX)
    val d = CSPOMConstraint('diffn)(x, y, dx, dy)

    replaceCtr(constraint, Seq(c1, c2, d), problem)

  }

}