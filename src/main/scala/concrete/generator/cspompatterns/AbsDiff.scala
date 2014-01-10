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
  type A = (CSPOMVariable, Set[CSPOMConstraint])

  override def mtch(c: CSPOMConstraint, problem: CSPOM) = c match {
    case CSPOMConstraint(result: CSPOMVariable, 'sub, args, _) if result.params("var_is_introduced") =>
      val process = problem.constraints(result).filter {
        case CSPOMConstraint(_, 'abs, Seq(result), _) => true
        case _ => false
      }
      if (process.isEmpty) {
        None
      } else {
        Some((result, process))
      }
    case _ => None
  }

  def compile(c: CSPOMConstraint, problem: CSPOM, data: (CSPOMVariable, Set[CSPOMConstraint])) = {
    data._2.foldLeft(Delta()) { (acc, fc) =>
      problem.removeConstraint(c);
      problem.removeConstraint(fc);
      val nc = problem.ctr(new CSPOMConstraint(
        fc.result, 'absdiff, c.arguments: _*));
      problem.removeVariable(data._1)
      acc.removed(c).removed(fc).added(nc)
    }

  }
}