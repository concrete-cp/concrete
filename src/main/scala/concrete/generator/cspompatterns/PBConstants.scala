package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import concrete.constraint.semantic.SumMode
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import SumMode._
import cspom.variable.IntExpression
import concrete.CSPOMDriver
import concrete.generator.constraint.SumGenerator
import cspom.variable.BoolExpression
import CSPOM._

/**
 *  Remove constants from linear constraints
 */
object PBConstants extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'pseudoboolean && {
      val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)
      vars
        .collectFirst {
          case c: CSPOMConstant[_] => Unit
        }
        .nonEmpty
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
        case (CSPOMConstant(c: Boolean), p) => if (c) p else 0
        case (CSPOMConstant(c: Int), p) =>
          require(c == 0 || c == 1)
          c * p
      }
      .sum

    vars match {
      case Seq() =>

        //logger.warn(s"Linear constraint with no variables: $constraint, entailed to $truth")
        val nr = reduceDomain(BoolExpression.coerce(constraint.result), SumConstants.checkConstant(const, mode))
        removeCtr(constraint, p) ++ replace(constraint.result, nr, p)

      case solverVariables =>
        val newConstraint =
          CSPOMConstraint(constraint.result)('pseudoboolean)(varCoefs, solverVariables, const) withParams
            constraint.params

        //println(s"replacing $constraint with $newConstraint")
        replaceCtr(constraint, newConstraint, p)
    }

  }

  def selfPropagation = false

}