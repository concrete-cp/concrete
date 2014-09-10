package concrete.generator.constraint;

import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import concrete.constraint.semantic.{ AllDifferent2C, AllDifferentBC }
import cspom.variable.CSPOMConstant
import Generator._
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.Element

final object ElementGenerator extends Generator {
  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit varMap: VarMap) = {
    val result = r.asVariable

    val varsIdx = for ((i, v) <- cspom2concreteIndexedSeq(constraint.arguments(0)))
      yield (i, v.asVariable)

    val index = cspom2concrete1D(constraint.arguments(1)).asVariable

    Element(result, index, varsIdx)

  }

}
