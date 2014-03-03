package concrete.generator.constraint;

import concrete.constraint.semantic.{ Eq, Add }
import concrete.constraint.Constraint
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import cspom.variable.IntVariable
import cspom.variable.FreeInt
import Generator._
import concrete.UNSATObject

final object AddGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val result = cspom2concreteVar(constraint.result)

    val Seq(v0, v1) = constraint.arguments map cspom2concreteVar

    if (completeVariables(result, v0, v1)) {
      Some(Seq(new Add(result, v0, v1)))
    } else {
      None
    }
  }

  private def completeVariables(result: Variable, v0: Variable, v1: Variable): Boolean = {
    undefinedVar(result, v0, v1) match {
      case Seq() => true;
      case Seq(`result`) =>
        result.dom = IntDomain(domainFrom(v0, v1, (_ + _)): _*);
        true
      case Seq(`v0`) =>
        v0.dom = IntDomain(domainFrom(result, v1, (_ - _)): _*);
        true
      case Seq(`v1`) =>
        v1.dom = IntDomain(domainFrom(result, v0, (_ - _)): _*);
        true
      case _ => false;
    }
  }

}
