package concrete.generator.cspompatterns

import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.SumLE
import concrete.constraint.linear.SumLT
import concrete.constraint.linear.SumNE
import concrete.generator.SumGenerator
import cspom.CSPOM
import cspom.CSPOM.constant
import cspom.CSPOM.constantSeq
import cspom.CSPOM.seq2CSPOMSeq
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMConstant

object SumFalse extends ConstraintCompilerNoData {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(CSPOMConstant(false), 'sum, _, _) => true
    case _ => false
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM) = {
    val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)

    val (revCoefs: Seq[Int], revConstant, revMode) = mode match {
      case SumNE => (coefs, const, SumEQ)
      case SumEQ => (coefs, const, SumNE)
      case SumLT => (coefs.map(-_), -const, SumLE)
      case SumLE => (coefs.map(-_), -const, SumLT)
    }

    replaceCtr(
      c,
      CSPOMConstraint('sum)(revCoefs, vars, revConstant) withParams c.params + ("mode" -> revMode.toString),
      p)
  }

  def selfPropagation = false

}