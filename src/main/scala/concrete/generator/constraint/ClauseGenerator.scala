package concrete.generator.constraint;

import Generator._
import concrete.Variable
import concrete.constraint.semantic.Clause
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable

final object ClauseGenerator extends Generator {

  override def gen(gC: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    require(gC.function == 'clause, "Unexpected constraint " + gC)
    val Seq(positive: CSPOMSeq[_], negative: CSPOMSeq[_]) = gC.arguments

    if (positive.exists {
      case CSPOMConstant(true) => true
      case _                   => false
    } || negative.exists {
      case CSPOMConstant(false) => true
      case _                    => false
    }) {
      Seq()
    } else {

      val posConc = positive.collect { case v: BoolVariable => cspom2concreteVar(v) }.toArray
      val negConc = negative.collect { case v: BoolVariable => cspom2concreteVar(v) }.toArray

      Seq(new Clause(posConc, negConc))
    }
  }

}
