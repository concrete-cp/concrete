package concrete.generator.constraint;

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concrete
import concrete.constraint.semantic.AbsAC
import concrete.constraint.semantic.AbsBC
import cspom.CSPOMConstraint

final object AbsGenerator extends Generator with LazyLogging {

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0: C21D) = constraint.arguments map cspom2concrete

    val Var(r) = result
    val Var(v) = v0
    Seq(new AbsAC(r, v), new AbsBC(r, v))
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