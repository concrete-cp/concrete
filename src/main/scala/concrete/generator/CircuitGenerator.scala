package concrete.generator

;

import cspom.CSPOMConstraint
import Generator._
import concrete.constraint.semantic.Circuit
import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq}
import concrete.constraint.semantic.AllDifferent2C
import concrete.constraint.semantic.AllDifferentBC

final class CircuitGenerator(adg: AllDifferentGenerator) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(list: CSPOMSeq[_], CSPOMConstant(start: Int), size: CSPOMExpression[_]) = constraint.arguments

    val vars = cspom2concreteSeq(list).map(_.asVariable(adg.pg)).toArray

    val s = cspom2concrete(size).asVariable(adg.pg)

    val c = new Circuit(vars, start, s)

    Seq(c, new AllDifferent2C(vars), new AllDifferentBC(vars))

  }

}
