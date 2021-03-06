package concrete.generator;

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concrete1D
import concrete.constraint.semantic.AbsAC
import concrete.constraint.semantic.AbsBC
import cspom.CSPOMConstraint

final class AbsGenerator(pg: ProblemGenerator) extends Generator with LazyLogging {

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0) = constraint.arguments.map(cspom2concrete1D(_))

    val r = result.asVariable(pg)
    val v= v0.asVariable(pg)
//
//    (result, v0) match {
//      case (Const(r: Int), Const(v: Int)) =>
//        require(r == math.abs(v))
//        Seq()
//      case (Const(r: Int), Var(v)) =>
//        require(v.initDomain.view.toSet == Set(-r, r))
//        Seq()
//      case (Var(r), Const(v: Int)) =>
//        require(r.initDomain.view.toSeq == Seq(math.abs(v)))
//        Seq()
//      case (Var(r), Var(v)) =>
        Seq(new AbsAC(r, v), new AbsBC(r, v))
//      case _ => throw new IllegalArgumentException("Abs only accept simple integer expressions")
//    }

    //    val (Var(result), Var(v0)) match {
    //      case (Var(result), Var(v0)) => Seq(new AbsAC(result, v0), new AbsBC(result, v0))
    //      case (Var(result), Const(c)) => {
    //        result.dom.assign(math.abs(c))
    //        Seq()
    //      }
    //      case (Const(r), Var(v0)) => {
    //        v0.dom.filterValues(v => r == math.abs(v))
    //        Seq()
    //      }
    //      case (Const(r), Const(c)) if (r == math.abs(c)) => Nil
    //      case _ =>
    //        logger.warn("Unconsistency detected during generation of " + constraint)
    //        throw UNSATObject
    //    }

  }

}
