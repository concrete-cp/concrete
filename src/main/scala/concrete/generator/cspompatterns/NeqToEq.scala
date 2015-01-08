package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression

object NeqToEq extends ConstraintCompilerNoData {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'ne
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM) = {
    var delta = Delta.empty
    val opposite: SimpleExpression[Boolean] = c.result match {
      case CSPOMConstant(true)  => CSPOMConstant(false)
      case CSPOMConstant(false) => CSPOMConstant(true)
      case v: CSPOMVariable[_] =>
        val aux = new BoolVariable()
        delta = delta.added(problem.ctr(new CSPOMConstraint(aux, 'not, Seq(v))))
        aux
    }

    delta ++ replaceCtr(c, new CSPOMConstraint(opposite, 'eq, c.arguments, c.params), problem)
  }

  def selfPropagation = false

}