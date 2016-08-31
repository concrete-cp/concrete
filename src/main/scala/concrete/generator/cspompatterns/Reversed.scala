package concrete.generator.cspompatterns

import concrete.CSPOMDriver
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
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.IntExpression
import cspom.compiler.Delta

object Reversed extends ConstraintCompiler {

  type A = Symbol

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMConstant(false), s, _, _) => s
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    PartialFunction.condOpt(data) {
      case 'eq =>
        require(c.arguments.size == 2)

        val args = c.arguments.map { case IntExpression(e) => e }
        CSPOMDriver.linear(args, Seq(1, -1), "ne", 0)
      case 'ne =>
        CSPOMConstraint('eq)(c.arguments: _*)

      case 'sum =>
        val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)

        val (revCoefs: Seq[Int], revConstant, revMode) = mode match {
          case SumNE => (coefs, const, SumEQ)
          case SumEQ => (coefs, const, SumNE)
          case SumLT => (coefs.map(-_), -const, SumLE)
          case SumLE => (coefs.map(-_), -const, SumLT)
        }
        CSPOMConstraint('sum)(revCoefs, vars, revConstant) withParams c.params + ("mode" -> revMode.toString)

    }
      .map(nc => replaceCtr(c, nc withParams c.params, p))
      .getOrElse(Delta.empty)
  }

}