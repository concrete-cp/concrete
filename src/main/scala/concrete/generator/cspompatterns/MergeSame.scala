package concrete.generator.cspompatterns
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.compiler.Delta

/**
 * Find and merge auxiliary variables similary defined by other constraints
 */
object MergeSame extends ConstraintCompiler {

  type A = CSPOMConstraint

  override def mtch(c: CSPOMConstraint, problem: CSPOM): Option[A] = {
    c.scope.flatMap(problem.constraints).collectFirst {
      case same @ CSPOMConstraint(_, c.function, c.arguments, c.params) if (same ne c) => same
    }

    //    c.scope.flatMap(problem.constraints).find(same => (same ne c) && same.function == c.function &&
    //      same.arguments == c.arguments && same.params == c.params)
  }

  def compile(c: CSPOMConstraint, problem: CSPOM, same: CSPOMConstraint) = {

    problem.removeConstraint(c)
    val eqC = new CSPOMConstraint('eq, c.result, same.result)
    problem.ctr(eqC)

    Delta().removed(c).added(eqC)
  }

}
