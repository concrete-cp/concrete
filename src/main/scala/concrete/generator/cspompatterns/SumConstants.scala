package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import concrete.constraint.linear.SumMode
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntExpression
import concrete.CSPOMDriver
import concrete.generator.constraint.SumGenerator
import cspom.variable.BoolExpression
import CSPOM._
import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.SumLT
import concrete.constraint.linear.SumLE
import concrete.constraint.linear.SumNE

/**
 *  Remove constants from linear constraints
 */
object SumConstants extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'sum && {
      val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)
      vars
        .collectFirst {
          case c: CSPOMConstant[_] => Unit
        }
        .nonEmpty
    }
  }

  def checkConstant(constant: Int, mode: SumMode): Boolean = {
    mode match {
      case SumEQ => 0 == constant
      case SumLT => 0 < constant
      case SumLE => 0 <= constant
      case SumNE => 0 != constant
    }
  }

  def compile(constraint: CSPOMConstraint[_], p: CSPOM) = {
    val (expr, coefs, originalConst, mode) = SumGenerator.readCSPOM(constraint)

    val (vars, varCoefs) = expr.zip(coefs)
      .collect {
        case (v: CSPOMVariable[_], p) => (v, p)
      }
      .unzip

    val const = originalConst - expr.zip(coefs)
      .collect {
        case (CSPOMConstant(c: Int), p) => c * p
      }
      .sum

    vars match {
      case Seq() =>

        //logger.warn(s"Linear constraint with no variables: $constraint, entailed to $truth")
        val nr = reduceDomain(BoolExpression.coerce(constraint.result), checkConstant(const, mode))
        removeCtr(constraint, p) ++ replace(constraint.result, nr, p)

      case solverVariables =>
        val newConstraint =
          CSPOMConstraint(constraint.result)('sum)(varCoefs, solverVariables, const) withParams
            constraint.params

        println(s"replacing $constraint with $newConstraint")
        replaceCtr(constraint, newConstraint, p)
    }

  }

  def selfPropagation = false

}