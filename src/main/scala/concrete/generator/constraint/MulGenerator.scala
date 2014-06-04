package concrete.generator.constraint;

import concrete.generator.FailedGenerationException
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import concrete.UNSATObject
import concrete.constraint.Residues
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.semantic.Sum
import Generator._
import concrete.constraint.semantic.ConstProd
import concrete.constraint.semantic.FilterSum
import concrete.constraint.semantic.MulAC
import concrete.constraint.semantic.MulBC

final object MulGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asInstanceOf[C21D]
    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    //    if (Seq(result, v0, v1) collect { case Var(v) if (v.dom.undefined) => v } match {
    //      case Seq() => true
    //      case Seq(v) if (result.is(v)) => {
    //        val values = domainFrom1D(Seq(v0, v1), genMul(_))
    //        v.dom = IntDomain(values: _*)
    //        true
    //      }
    //      case Seq(v) if (v0.is(v)) => {
    //        v.dom = IntDomain(generateValues(result, v1): _*)
    //        true
    //      }
    //      case Seq(v) if (v1.is(v)) => {
    //        v.dom = IntDomain(generateValues(result, v0): _*)
    //        true
    //      }
    //      case _ => false
    //
    //    }) {
    (result, v0, v1) match {
      case (Const(r), Const(v0), Const(v1)) =>
        if (r == v0 * v1) Nil else throw UNSATObject
      case (Const(r), Var(v0), Var(v1)) => Seq(
        new ConstProd(v0, v1, r))
      case (Var(r), Const(v0), Var(v1)) => Seq(
        new Sum(0, Array(-1, v0), Array(r, v1), FilterSum.SumEQ))
      case (Var(r), Var(v0), Const(v1)) => Seq(
        new Sum(0, Array(-1, v1), Array(r, v0), FilterSum.SumEQ))

      case (Var(r), Var(v0), Var(v1)) =>
        Seq(new MulAC(r, v0, v1, true), new MulBC(r, v0, v1))
    }

  }
//
//  private def genMul: Function[Seq[Int], Int] = { case Seq(i, j) => i * j }
//
//  private def genDivP: PartialFunction[Seq[Int], Int] = {
//    case Seq(i, j) if (i % j == 0) => i / j
//  }
//
//  private def genDiv(s: Seq[Int]) = genDivP.lift(s).toIterable
//
//  private def generateValues(result: C21D, variable: C21D) = {
//    domainFromFlat1D(Seq(result, variable), genDiv(_))
//  }

}
