package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import concrete.constraint.semantic.SumMode
import concrete.constraint.semantic.SumNE
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import SumMode._
import cspom.variable.IntExpression
import concrete.CSPOMDriver
import concrete.generator.constraint.SumGenerator

/**
 *  Remove constants from linear constraints
 */
object SumConstants extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'sum && c.result.isTrue && {
      val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)
      vars
        .collectFirst {
          case c: CSPOMConstant[_] => Unit
        }
        .nonEmpty
    }
  }

  def compile(constraint: CSPOMConstraint[_], p: CSPOM) = {
    val (expr, coefs, const, mode) = SumGenerator.readCSPOM(constraint)

    val (vars, varCoefs) = expr.zip(coefs)
      .collect {
        case (v: CSPOMVariable[Int], p) => (v, p)
      }
      .unzip

    val constant = const - expr.zip(coefs)
      .collect {
        case (CSPOMConstant(c: Int), p) => c * p
      }
      .sum

    vars match {
      case Seq() =>
        mode match {
          case SumEQ => require(constant == 0)
          case SumLT => require(constant > 0)
          case SumLE => require(constant >= 0, s"inconsistent sum $constraint")
          case SumNE => require(constant != 0)
        }
        removeCtr(constraint, p)

      case IntExpression.seq(solverVariables) =>
        val newConstraint =
          CSPOMDriver.linear(CSPOMSeq(vars: _*), varCoefs, mode.toString(), constant) withParams
            (constraint.params - "mode")

        //println(s"replacing $constraint with $newConstraint")
        replaceCtr(constraint, newConstraint, p)
    }

  }

  def selfPropagation = false

}