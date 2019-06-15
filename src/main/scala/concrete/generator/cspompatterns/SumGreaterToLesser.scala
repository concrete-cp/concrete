package concrete.generator.cspompatterns

import concrete.constraint.linear.SumMode
import concrete.generator.SumGenerator
import cspom.CSPOM._
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.{CSPOMConstant, IntExpression}
import cspom.{CSPOM, CSPOMConstraint}

/**
  * Remove constants from linear constraints
  */
object SumGreaterToLesser extends ConstraintCompiler {
  type A = SumMode

  def functions = Functions("sum")

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[SumMode] = {
    SumGenerator.readMode(c)
      .filter(mode => mode == SumMode.GE || mode == SumMode.GT)
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, mode: A): Delta = {
    val Seq(
    IntExpression.constSeq(constants),
    vars,
    CSPOMConstant(k: Int)) = c.arguments

    val newMode = mode match {
      case SumMode.GE => SumMode.LE
      case SumMode.GT => SumMode.LT
    }


    val newConstraint = CSPOMConstraint(c.result)("sum")(constants.map(-_), vars, -k) withParams (c.params + ("mode" -> newMode))

    replaceCtr(c, newConstraint, p)


  }

}