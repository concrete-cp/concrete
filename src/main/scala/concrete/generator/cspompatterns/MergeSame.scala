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

  def mtch(c: CSPOMConstraint, problem: CSPOM) = {
    c.scope.flatMap(problem.constraints).find(same => (same ne c) && same.function == c.function &&
      same.arguments == c.arguments)
  }

  def compile(c: CSPOMConstraint, problem: CSPOM, same: CSPOMConstraint) = {

    problem.removeConstraint(c)
    val eqC = new CSPOMConstraint("eq", c.result, same.result)
    problem.addConstraint(eqC)

    Delta(Seq(c), (c.result.flattenVariables ++ same.result.flattenVariables).toSet)
  }

}
