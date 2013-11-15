package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.variable.BoolVariable
import cspom.compiler.Delta
import cspom.variable.CSPOMFalse
import cspom.variable.CSPOMVariable

object SplitAllEq extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint, problem: CSPOM) =
    constraint.function == 'eq && constraint.arguments.size > 2

  def compile(constraint: CSPOMConstraint, problem: CSPOM) = {

    problem.removeConstraint(constraint)

    constraint.result match {
      case CSPOMTrue =>
        for (Seq(a, b) <- constraint.arguments.sliding(2)) {
          problem.ctr(new CSPOMConstraint('eq, a, b))
        }
        Delta(Seq(constraint), constraint.scope)
      case CSPOMFalse =>
        val rs = for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
          val r = CSPOMVariable.bool()
          problem.ctr(new CSPOMConstraint(r, 'ne, a, b))
          r
        }
        problem.ctr(new CSPOMConstraint('or, rs.toSeq: _*))
        Delta(Seq(constraint), constraint.scope ++ rs)
      case r: BoolVariable =>
        val rs = for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
          val r = CSPOMVariable.bool()
          problem.ctr(new CSPOMConstraint(r, 'eq, a, b))
          r
        }
        problem.ctr(new CSPOMConstraint(r, 'and, rs.toSeq: _*))
        Delta(Seq(constraint), constraint.scope ++ rs + r)

    }

  }
}
