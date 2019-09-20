package concrete.generator

;

import concrete.constraint.semantic.{AllDifferent2C, AllDifferentAC, AllDifferentBC, FZSubcircuit, NeqC}
import concrete.generator.Generator._
import cspom.CSPOMConstraint
import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq}

final class CircuitGenerator(adg: AllDifferentGenerator) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(list: CSPOMSeq[_], CSPOMConstant(offset: Int)) = constraint.arguments

    val vars = cspom2concreteSeq(list).map(_.asVariable(adg.pg)).toArray

    // val s = cspom2concrete(size).asVariable(adg.pg)

    //val c = new XCSPCircuit(vars, start, s)
    val c = Seq(new FZSubcircuit(vars, offset))

    c ++: Seq(new AllDifferentAC(vars), new AllDifferent2C(vars))

  }

}
