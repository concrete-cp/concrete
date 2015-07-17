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
import cspom.variable.BoolExpression

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
        val truth = mode match {
          case SumEQ => constant == 0
          case SumLT => 0 > constant
          case SumLE => 0 >= constant
          case SumNE => constant != 0
        }
        logger.warn(s"Linear constraint with no variables: $constraint, entailed to $truth")
        val nr = reduceDomain(BoolExpression.coerce(constraint.result), truth)
        removeCtr(constraint, p) ++ replace(constraint.result, nr, p)

      case solverVariables =>
        val newConstraint =
          CSPOMConstraint(constraint.result)('sum)(CSPOMConstant.ofSeq(varCoefs), CSPOMSeq(solverVariables: _*), CSPOMConstant(constant)) withParams
            constraint.params

        //println(s"replacing $constraint with $newConstraint")
        replaceCtr(constraint, newConstraint, p)
    }

  }

  def selfPropagation = false

}