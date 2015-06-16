package concrete.generator.constraint;

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concrete1D
import concrete.constraint.semantic.AbsAC
import concrete.constraint.semantic.AbsBC
import cspom.CSPOMConstraint

final object AbsGenerator extends Generator with LazyLogging {

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0) = constraint.arguments.map(cspom2concrete1D(_))
    
    (result, v0) match {
      case (Const(r), Const(v)) =>
        require(r == math.abs(v))
        Seq()
      case (Const(r), Var(v)) =>
        require(v.initDomain.sameElements(Seq(-r, r)))
        Seq()
      case (Var(r), Const(v)) =>
        require(r.initDomain.sameElements(Seq(math.abs(v))))
        Seq()
      case (Var(r), Var(v)) =>
        Seq(new AbsAC(r, v), new AbsBC(r, v))
      case _ => throw new IllegalArgumentException("Abs only accept simple expressions")
    }

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
