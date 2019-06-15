package concrete.generator.cspompatterns

import concrete.generator.SumGenerator
import cspom.CSPOM.{constant, constantSeq, seq2CSPOMSeq}
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompilerNoData, Delta, Functions}
import cspom.variable.{BoolExpression, CSPOMConstant, CSPOMVariable}
import cspom.{CSPOM, CSPOMConstraint}

/**
  * Remove constants from linear constraints
  */
object PBConstants extends ConstraintCompilerNoData {

  def functions = Functions("pseudoboolean")

  def matchBool(c: CSPOMConstraint[_], p: CSPOM): Boolean = {
    val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)
    vars
      .collectFirst {
        case c: CSPOMConstant[_] => ()
      }
      .nonEmpty
  }

  def compile(constraint: CSPOMConstraint[_], p: CSPOM): Delta = {
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
          CSPOMConstraint(constraint.result)("pseudoboolean")(varCoefs, solverVariables, const) withParams
            constraint.params

        //println(s"replacing $constraint with $newConstraint")
        replaceCtr(constraint, newConstraint, p)
    }

  }

  def selfPropagation = false

}