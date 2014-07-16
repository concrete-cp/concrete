package concrete.generator.constraint;

import concrete.constraint.semantic.Abs
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import scala.collection.immutable.SortedSet
import cspom.CSPOMConstraint
import concrete.UNSAT
import concrete.UNSATObject
import Generator._

final object AbsGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0: C21D) = constraint.arguments map cspom2concrete

    (result, v0) match {
      case (Var(result), Var(v0)) => Seq(new Abs(result, v0))
      case _ => Seq()
    }

  }

}
