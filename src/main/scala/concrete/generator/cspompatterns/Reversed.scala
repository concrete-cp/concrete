package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import concrete.constraint.linear.SumMode._
import concrete.generator.SumGenerator
import cspom.CSPOM.{constant, constantSeq, seq2CSPOMSeq}
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.{BoolVariable, CSPOMConstant}

object Reversed extends ConstraintCompiler {

  type A = String

  def functions = Functions("eq", "ne", "sum")

  override def constraintMatcher: PartialFunction[CSPOMConstraint[_], String] = {
    case CSPOMConstraint(CSPOMConstant(false), s, _, _) => s
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A): Delta = {
    val nc = data match {
      case "eq" =>
        val bools = (for (Seq(a, b) <- c.arguments.sliding(2)) yield {
          CSPOMConstraint(new BoolVariable())("ne")(a, b)
        }).toSeq

        CSPOMDriver.clause(bools.map(_.result): _*)() +: bools

      case "ne" =>
        require(c.arguments.size == 2)
        Seq(CSPOMConstraint("eq")(c.arguments: _*))

      case "sum" =>
        val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)

        val (revCoefs: Seq[Int], revConstant, revMode) = mode match {
          case NE => (coefs, const, EQ)
          case EQ => (coefs, const, NE)
          case LT => (coefs.map(-_), -const, LE)
          case LE => (coefs.map(-_), -const, LT)
        }
        Seq(CSPOMConstraint("sum")(revCoefs, vars, revConstant) withParam "mode" -> revMode.toString)

    }

    ConstraintCompiler.replaceCtr(c, nc, p)
  }

}