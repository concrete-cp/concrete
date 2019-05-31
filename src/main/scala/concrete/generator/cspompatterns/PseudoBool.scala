package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.variable.BoolExpression
import cspom.variable.CSPOMSeq

object PseudoBool extends ConstraintCompilerNoData {

  def functions = Functions('sum)

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM): Boolean = c match {
    case CSPOMConstraint(_, _, Seq(CSPOMSeq(args), _), _) => args.forall {
      case BoolExpression(e)           => true
      case e if BoolExpression.is01(e) => true
      case _                           => false
    }
    case _ => false
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM): Delta = {

    ConstraintCompiler.replaceCtr(
      c,
      new CSPOMConstraint(c.result, 'pseudoboolean, c.arguments, c.params),
      p)
  }

  def selfPropagation = false

}
