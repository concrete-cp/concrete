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

/**
 *  Remove constants from linear constraints
 */
object SumConstants extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'sum && {
      val Seq(CSPOMSeq(vars), _) = c.arguments
      vars
        .collectFirst {
          case c: CSPOMConstant[_] => Unit
        }
        .nonEmpty
    }
  }

  def compile(constraint: CSPOMConstraint[_], p: CSPOM) = {
    val Seq(CSPOMSeq(vars), CSPOMConstant(const: Int)) = constraint.arguments //map cspom2concreteVar

    val params = constraint.params.get("coefficients") match {
      case Some(p: Seq[_]) => p.asInstanceOf[Seq[Int]]
      case None            => Seq.fill(vars.length)(1)
      case _               => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
    }

    val (solverVariables, varParams) = vars.zip(params)
      .collect {
        case (v: CSPOMVariable[_], p) => (v, p)
      }
      .unzip

    val constant = const - vars.zip(params)
      .collect {
        case (CSPOMConstant(c: Int), p) => c * p
      }
      .sum

    val mode = constraint.params.get("mode").collect {
      case m: String => SumMode.withName(m)
    }.get

    if (solverVariables.isEmpty) {
      mode match {
        case SumEQ => require(constant == 0)
        case SumLT => require(constant > 0)
        case SumLE => require(constant >= 0, s"inconsistent sum $constraint")
        case SumNE => require(constant != 0)
      }
      removeCtr(constraint, p)
    } else {
      replaceCtr(
        constraint,
        CSPOMConstraint(
          'sum,
          Seq(CSPOMSeq(solverVariables: _*), CSPOMConstant(constant)),
          constraint.params + ("coefficients" -> varParams)),
        p)
    }
  }

  def selfPropagation = false

}