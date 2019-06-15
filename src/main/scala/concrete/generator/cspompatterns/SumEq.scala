package concrete.generator.cspompatterns

import concrete.constraint.linear.SumMode
import concrete.generator.SumGenerator
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.CSPOMExpression
import cspom.{CSPOM, CSPOMConstraint}

object SumEq extends ConstraintCompiler {

  type A = (CSPOMExpression[_], Seq[CSPOMExpression[_]])

  def functions = Functions("sum")

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[(CSPOMExpression[Any], Seq[CSPOMExpression[Any]])] = {
    val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(c)
    if (mode == SumMode.EQ && constant == 0 && (coefs == Seq(1, -1) || coefs == Seq(-1, 1))) {
      Some((c.result, vars))
    } else {
      None
    }
  }

  //    c match {
  //      case CSPOMConstraint(r, "sum", Seq(IntExpression.constSeq(coefs), CSPOMSeq(args), CSPOMConstant(0)), p) if p.get("mode").exists(m => m == "eq" || m == SumEq) && () =>
  //        Some((r, args))
  //      case _ => None
  //    }
  //  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A): Delta = {
    val (r, args) = data
    ConstraintCompiler.replaceCtr(
      c,
      CSPOMConstraint(r)("eq")(args: _*) withParams (c.params - "mode" - "constant"),
      p)
  }

  def selfPropagation = false

}