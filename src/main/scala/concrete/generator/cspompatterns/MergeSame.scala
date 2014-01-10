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

  def mtch = findSame andThen {
    case Some(same: CSPOMConstraint) => same
  }

  val findSame: PartialFunction[(CSPOMConstraint, CSPOM), Option[A]] = {
    case (c, problem) =>
      c.scope.flatMap(problem.constraints(_)).find { same =>
        (same ne c) && same.function == c.function &&
          same.arguments == c.arguments && same.params == c.params
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
