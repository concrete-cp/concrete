package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
object AbsDiff extends ConstraintCompiler {
  type A = Set[CSPOMConstraint]

  override def mtch(c: CSPOMConstraint, problem: CSPOM) = c match {
    case CSPOMConstraint(result: CSPOMVariable, 'sub, args, _) if result.params("var_is_introduced") =>
      val process = problem.constraints(result).collect {
        case c @ CSPOMConstraint(_, 'abs, Seq(result), _) => c
      }
      if (process.isEmpty) {
        None
      } else {
        Some(process)
      }
    case _ => None
  }

  def compile(c: CSPOMConstraint, problem: CSPOM, data: Set[CSPOMConstraint]) = {
    data.foldLeft(Delta()) { (acc, fc) =>
      val nc = new CSPOMConstraint(fc.result, 'absdiff, c.arguments: _*)
      acc ++ replaceCtr(Seq(c, fc), nc, problem)
    }

  }
}