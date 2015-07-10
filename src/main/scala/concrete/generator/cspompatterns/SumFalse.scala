package concrete.generator.cspompatterns

import scala.reflect.runtime.universe

import concrete.constraint.semantic.SumMode.SumEQ
import concrete.constraint.semantic.SumMode.SumLE
import concrete.constraint.semantic.SumMode.SumLT
import concrete.constraint.semantic.SumMode.SumNE
import concrete.generator.constraint.SumGenerator
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMConstant

object SumFalse extends ConstraintCompilerNoData {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(CSPOMConstant(false), 'sum, _, _) => true
    case _ => false
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM) = {
    val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(c)

    val (revCoefs: Seq[Int], revConstant, revMode) = mode match {
      case SumNE => (coefs, constant, SumEQ)
      case SumEQ => (coefs, constant, SumNE)
      case SumLT => (coefs.map(-_), -constant, SumLE)
      case SumLE => (coefs.map(-_), -constant, SumLT)
    }

    replaceCtr(
      c,
      CSPOMConstraint('sum)(CSPOMConstant.ofSeq(revCoefs), vars, CSPOMConstant(revConstant)) withParams c.params + ("mode" -> revMode.toString),
      p)
  }

  def selfPropagation = false

}