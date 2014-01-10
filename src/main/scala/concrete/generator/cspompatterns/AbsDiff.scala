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

  val constraintWithAbs: PartialFunction[(CSPOMConstraint, CSPOM), (CSPOMVariable, Set[CSPOMConstraint])] = {
    case (CSPOMConstraint(result: CSPOMVariable, 'sub, args, _), problem) if result.params("var_is_introduced") =>
      (result, problem.constraints(result).collect {
        case c @ CSPOMConstraint(_, 'abs, Seq(`result`), _) => c
      })
  }

  def mtch = constraintWithAbs andThen {
    case (result, process) if process.nonEmpty => (result, process)
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