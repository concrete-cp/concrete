package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import concrete.constraint.linear.{SumEQ, SumLE, SumLT, SumNE}
import concrete.generator.SumGenerator
import cspom.CSPOM.{constant, constantSeq, seq2CSPOMSeq}
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, Delta}
import cspom.variable.{BoolVariable, CSPOMConstant, CSPOMSeq, IntExpression}

object Reversed extends ConstraintCompiler {

  type A = Symbol

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMConstant(false), s, _, _) => s
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    PartialFunction.condOpt(data) {
      case 'eq =>
        val bools = (for (Seq(a, b) <- c.arguments.sliding(2)) yield {
          CSPOMConstraint(new BoolVariable())('ne)(a, b)
        }).toSeq

        CSPOMDriver.clause(bools.map(_.result): _*)() +: bools
//
//        val args = c.arguments.map { case IntExpression(e) => e }
//        CSPOMDriver.linear(args, Seq(1, -1), "ne", 0)
      case 'ne =>
        require(c.arguments.size == 2)
        Seq(CSPOMConstraint('eq)(c.arguments: _*))

      case 'sum =>
        val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)

        val (revCoefs: Seq[Int], revConstant, revMode) = mode match {
          case SumNE => (coefs, const, SumEQ)
          case SumEQ => (coefs, const, SumNE)
          case SumLT => (coefs.map(-_), -const, SumLE)
          case SumLE => (coefs.map(-_), -const, SumLT)
        }
        Seq(CSPOMConstraint('sum)(revCoefs, vars, revConstant) withParam "mode" -> revMode.toString)

    }
      .map(nc => replaceCtr(c, nc, p))
      .getOrElse(Delta.empty)
  }

}