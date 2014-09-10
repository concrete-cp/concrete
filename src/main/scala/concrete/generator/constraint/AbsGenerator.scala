package concrete.generator.constraint;

import concrete.constraint.semantic.Abs
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import scala.collection.immutable.SortedSet
import cspom.CSPOMConstraint
import concrete.UNSAT
import concrete.UNSATObject
import Generator._
import com.typesafe.scalalogging.LazyLogging

final object AbsGenerator extends Generator with LazyLogging {

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0: C21D) = constraint.arguments map cspom2concrete

    (result, v0) match {
      case (Var(result), Var(v0)) => Seq(new Abs(result, v0))
      case (Var(result), Const(c)) => {
        result.dom.assign(math.abs(c))
        Seq()
      }
      case (Const(r), Var(v0)) => {
        v0.dom.filterValues(v => r == math.abs(v))
        Seq()
      }
      case (Const(r), Const(c)) if (r == math.abs(c)) => Nil
      case _ =>
        logger.warn("Unconsistency detected during generation of " + constraint)
        throw UNSATObject
    }

  }

}
