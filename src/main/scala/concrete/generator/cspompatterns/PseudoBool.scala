package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.SimpleExpression
import cspom.variable.BoolExpression
import cspom.compiler.ConstraintCompilerNoData

object PseudoBool extends ConstraintCompilerNoData {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(_, 'sum, Seq(CSPOMSeq(args), _), _) => args.forall {
      case BoolExpression(e)           => true
      case e if BoolExpression.is01(e) => true
      case _                           => false
    }
    case _ => false
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM) = {

    replaceCtr(
      c,
      new CSPOMConstraint(c.result, 'pseudoboolean, c.arguments, c.params),
      p)
  }

  def selfPropagation = false

}