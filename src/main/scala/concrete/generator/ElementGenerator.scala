package concrete.generator;

import Generator.cspom2concrete1D
import Generator.cspom2concreteIndexedSeq
import concrete.constraint.Constraint
import concrete.constraint.semantic.Element
import cspom.CSPOMConstraint

final class ElementGenerator(pg: ProblemGenerator) extends Generator {
  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit varMap: VarMap): Seq[Constraint] = {
    val result = r.asVariable(pg)

    val varsIdx = for ((i, v) <- cspom2concreteIndexedSeq(constraint.arguments.head))
      yield (i, v.asVariable(pg))

    val index = cspom2concrete1D(constraint.arguments(1)).asVariable(pg)

    Element(result, index, varsIdx)

  }

}
