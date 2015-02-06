package concrete.generator.constraint;

import concrete.constraint.semantic.Clause
import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMConstant
import cspom.variable.BoolVariable
import Generator._
import cspom.variable.CSPOMSeq

final object ClauseGenerator extends Generator {

  override def gen(gC: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    require(gC.function == 'clause, "Unexpected constraint " + gC)
    val Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]) = gC.arguments

    val posConc = positive.map(cspom2concreteVar).toArray
    val negConc = negative.map(cspom2concreteVar).toArray

    Seq(new Clause(posConc, negConc))
  }

}
