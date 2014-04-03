package concrete.generator.constraint;

import concrete.constraint.Constraint
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import cspom.variable.IntVariable
import cspom.variable.FreeInt
import Generator._
import concrete.UNSATObject
import concrete.constraint.semantic.AddBC
import concrete.constraint.semantic.AddAC

final object AddGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val result = cspom2concreteVar(constraint.result)

    val Seq(v0, v1) = constraint.arguments map cspom2concreteVar

    if (completeVariables(result, v0, v1)) {
      Some(Seq(new AddBC(result, v0, v1), new AddAC(result, v0, v1, true)))
    } else {
      None
    }
  }

  private def completeVariables(result: Variable, v0: Variable, v1: Variable): Boolean = {
    undefinedVar(result, v0, v1) match {
      case Seq() => true;
      case Seq(`result`) =>
        result.dom =
          if (v0.dom.boundVal && v1.dom.boundVal) {
            IntDomain(v0.dom.valueInterval + v1.dom.valueInterval)
          } else {
            IntDomain(domainFrom(v0, v1, (_ + _)): _*)
          }
        true
      case Seq(`v0`) =>
        v0.dom =
          if (result.dom.boundVal && v1.dom.boundVal) {
            IntDomain(result.dom.valueInterval - v1.dom.valueInterval)
          } else {
            IntDomain(domainFrom(result, v1, (_ - _)): _*);
          }
        true
      case Seq(`v1`) =>
        v1.dom =
          if (result.dom.boundVal && v0.dom.boundVal) {
            IntDomain(result.dom.valueInterval - v0.dom.valueInterval)
          } else {
            IntDomain(domainFrom(result, v0, (_ - _)): _*);
          }
        true
      case _ => false;
    }
  }

}
