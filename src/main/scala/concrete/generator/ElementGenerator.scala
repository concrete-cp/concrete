package concrete.generator;

import Generator.cspom2concrete1D
import Generator.cspom2concreteIndexedSeq
import concrete.constraint.semantic.Element
import cspom.CSPOMConstraint

final object ElementGenerator extends Generator {
  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit varMap: VarMap) = {
    val result = r.asVariable

    val varsIdx = for ((i, v) <- cspom2concreteIndexedSeq(constraint.arguments(0)))
      yield (i, v.asVariable)

    val index = cspom2concrete1D(constraint.arguments(1)).asVariable

    Element(result, index, varsIdx)

  }

}
