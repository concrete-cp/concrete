package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData}
import cspom.variable.BoolExpression
import cspom.variable.CSPOMSeq

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

    ConstraintCompiler.replaceCtr(
      c,
      new CSPOMConstraint(c.result, 'pseudoboolean, c.arguments, c.params),
      p)
  }

  def selfPropagation = false

}
