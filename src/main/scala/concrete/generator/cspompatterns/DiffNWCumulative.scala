package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta}
import cspom.variable.{CSPOMSeq, IntExpression, SimpleExpression}


object DiffNWCumulative extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = constraint.function == 'diffn

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val Seq(CSPOMSeq(xs), CSPOMSeq(ds)) = constraint.arguments
    val xtransp: Seq[Seq[SimpleExpression[Int]]] = xs
      .map {
        case IntExpression.simpleSeq(x) => x
      }.transpose

    val dtransp: Seq[Seq[SimpleExpression[Int]]] = ds
      .map {
        case IntExpression.simpleSeq(d) => d
      }
      .transpose

    implicit def cspom: CSPOM = problem

    val Seq(x, y) = xtransp
    val Seq(dx, dy) = dtransp

    val minX = CSPOMSeqOperations(x).cmin
    val maxX = CSPOMSeqOperations((x, dx).zipped.map(_ + _)).cmax

    val minY = CSPOMSeqOperations(y).cmin
    val maxY = CSPOMSeqOperations((y, dy).zipped.map(_ + _)).cmax

    val c1 = cumulative(x, dx, dy, maxY - minY)
    val c2 = cumulative(y, dy, dx, maxX - minX)
    // val d = CSPOMConstraint('diffn)(xs, ds)

    ConstraintCompiler.addCtr(Seq(c1, c2), problem)

  }

}