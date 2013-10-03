package concrete.generator.constraint;

import concrete.constraint.semantic.{ Eq, Add }
import concrete.constraint.Constraint
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import cspom.variable.IntVariable
import cspom.variable.FreeInt
import AbstractGenerator._
import concrete.UNSATObject

final class AddGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc) = {

    val Seq(cspomResult, cspomV0, cspomV1) = constraint match {
      case CSPOMConstraint(r, "sub", Seq(v0, v1), _) => Seq(v0, r, v1)
      case CSPOMConstraint(r, "add", args, _) => r +: args
      case _ => throw new UnsupportedOperationException
    }

    val result = cspom2concrete1D(cspomResult)
    val Seq(v0: C21D, v1: C21D) = Seq(cspomV0, cspomV1) map cspom2concrete

    //map cspom2concrete

    if (Seq(result, v0, v1) collect { case C2V(v) if (v.dom.undefined) => v } match {
      case Seq() => true;
      case Seq(v) if result.is(v) =>
        val values = AbstractGenerator.domainFrom(Seq(v0, v1), { case Seq(i, j) => i + j });
        v.dom = IntDomain(values: _*);
        true
      case Seq(v) if v0.is(v) =>
        v.dom = IntDomain(generateValues(result, v1): _*);
        true
      case Seq(v) if v1.is(v) =>
        v.dom = IntDomain(generateValues(result, v0): _*);
        true
      case _ => false;
    }) {

      val constraint = (result, v0, v1) match {
        case (C2C(r), C2C(v0), C2C(v1)) => if (r == v0 + v1) None else throw UNSATObject

        case (C2V(r), C2C(v0), C2C(v1)) =>
          restrictDomain(r, Seq(v0 + v1)); None
        case (C2C(r), C2V(v0), C2C(v1)) =>
          restrictDomain(v0, Seq(r - v1)); None
        case (C2C(r), C2C(v0), C2V(v1)) =>
          restrictDomain(v1, Seq(r - v0)); None

        case (C2C(r), C2V(v0), C2V(v1)) => Some(new Eq(true, v0, r, v1))
        case (C2V(r), C2C(v0), C2V(v1)) => Some(new Eq(false, v1, v0, r))
        case (C2V(r), C2V(v0), C2C(v1)) => Some(new Eq(false, v0, v1, r))

        case (C2V(r), C2V(v0), C2V(v1)) => Some(new Add(r, v0, v1))
      }

      constraint.foreach(addConstraint)

      true

    } else {
      false
    }
  }

  def generateValues(result: C21D, variable: C21D) =
    AbstractGenerator.domainFrom(Seq(result, variable), { case Seq(i, j) => i - j })

}
