package concrete.generator.cspompatterns

import concrete.constraint.linear.SumMode._
import concrete.constraint.linear._
import concrete.generator.SumGenerator
import cspom.CSPOM._
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompilerNoData, Delta, Functions}
import cspom.variable.{BoolExpression, CSPOMConstant}
import cspom.{CSPOM, CSPOMConstraint}

/**
  * Remove constants from linear constraints
  */
object SumConstants extends ConstraintCompilerNoData {

  def functions = Functions('sum)

  def matchBool(c: CSPOMConstraint[_], p: CSPOM): Boolean = {
    val vars = SumGenerator.readCSPOM(c)._1
    vars.exists(_.isConstant)
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
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
        removeCtr(constraint, problem) ++ replace(constraint.result, nr, problem)

      case solverVariables =>
        val newConstraint =
          CSPOMConstraint(constraint.result)('sum)(varCoefs, solverVariables, const) withParams
            constraint.params

        //println(s"replacing $constraint with $newConstraint")
        replaceCtr(constraint, newConstraint, problem)
    }

  }

  def checkConstant(constant: Int, mode: SumMode): Boolean = {
    mode match {
      case EQ => 0 == constant
      case LT => 0 < constant
      case LE => 0 <= constant
      case NE => 0 != constant
    }
  }

  def selfPropagation = false

}