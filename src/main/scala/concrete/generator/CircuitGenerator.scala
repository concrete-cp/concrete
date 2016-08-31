package concrete.generator;

import cspom.CSPOMConstraint
import Generator._
import concrete.constraint.semantic.Circuit
import cspom.variable.CSPOMSeq
import concrete.constraint.semantic.AllDifferent2C
import concrete.constraint.semantic.AllDifferentBC

final class CircuitGenerator(adg: AllDifferentGenerator) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(s: CSPOMSeq[_]) = constraint.arguments

    val vars = s.withIndex.map {
      case (v, i) => i -> cspom2concrete(v).asVariable(adg.pg)
    }

    val c = Circuit(vars: _*)

    Seq(c, new AllDifferent2C(c.scope), new AllDifferentBC(c.scope: _*))

  }

}
