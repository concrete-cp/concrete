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

  override def genFunctional(constraint: CSPOMConstraint, result: C2Conc)(implicit variables: VarMap) = {
    val Seq(v0: C21D) = constraint.arguments map cspom2concrete

    (result, v0) match {
      case (Const(result), Const(v0)) =>
        if (result == math.abs(v0)) { Some(Nil) } else { throw UNSATObject }
      case (Const(result), Var(v0)) =>
        restrictDomain(v0, Seq(result, -result))
        Some(Nil)
      case (Var(result), Const(v0)) =>
        restrictDomain(result, Seq(math.abs(v0)))
        Some(Nil)
      case (Var(result), Var(v0)) =>
        if (v0.dom.undefined && result.dom.undefined) {
          None
        } else {
          if (!v0.dom.undefined) {
            restrictDomain(result, v0.dom.values.map(math.abs))
          }
          if (!result.dom.undefined) {
            restrictDomain(v0, result.dom.values.flatMap(v => Seq(-v, v)))
          }

          Some(Seq(new Abs(result, v0)))
        }
      case _ => None

    }

  }

}
