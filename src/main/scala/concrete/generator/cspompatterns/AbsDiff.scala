package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.CSPOMExpression

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
object AbsDiff extends ConstraintCompiler {
  type A = Set[CSPOMConstraint[Int]]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = c match {
    case CSPOMConstraint(result: CSPOMVariable[Int], 'sub, args, _) if result.hasParam("var_is_introduced") =>
      val process: Set[CSPOMConstraint[Int]] = problem.constraints(result).collect {
        case c @ CSPOMConstraint(_, 'abs, Seq(result), _) => c.asInstanceOf[CSPOMConstraint[Int]]
      }
      if (process.isEmpty) {
        None
      } else {
        Some(process)
      }
    case _ => None
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    data.foldLeft(Delta()) { (acc, fc) =>
      val nc = CSPOMConstraint(fc.result, 'absdiff, c.arguments: _*)
      acc ++ replaceCtr(Seq(c, fc), nc, problem)
    }

  }

  def selfPropagation = false
}