package concrete.generator.cspompatterns

import concrete.constraint.linear._
import concrete.generator.SumGenerator
import cspom.CSPOM._
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData}
import cspom.variable.{BoolExpression, CSPOMConstant}
import ConstraintCompiler._

/**
  * Remove constants from linear constraints
  */
object SumConstants extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'sum && {
      val vars = SumGenerator.readCSPOM(c)._1
      vars.exists(_.isConstant)
    }
  }

  def compile(constraint: CSPOMConstraint[_], p: CSPOM) = {
    val (expr, coefs, originalConst, mode) = SumGenerator.readCSPOM(constraint)

    val (vars, varCoefs) = expr.zip(coefs)
      .collect {
        case (v, p) if v.searchSpace > 1 => (v, p)
      }
      .unzip

    val const = originalConst - expr.zip(coefs)
      .collect {
        case (c: CSPOMConstant[_], p) => c.intValue * p
      }
      .sum

    vars match {
      case Seq() =>
        //        println(constraint)
        //        println(const, mode)
        //logger.warn(s"Linear constraint with no variables: $constraint, entailed to $truth")
        val nr = reduceDomain(BoolExpression.coerce(constraint.result), checkConstant(const, mode))
        removeCtr(constraint, p) ++ replace(constraint.result, nr, p)

      case solverVariables =>
        val newConstraint =
          CSPOMConstraint(constraint.result)('sum)(varCoefs, solverVariables, const) withParams
            constraint.params

        //println(s"replacing $constraint with $newConstraint")
        replaceCtr(constraint, newConstraint, p)
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

  def selfPropagation = false

}