package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMSeq
import cspom.util.Interval
import cspom.variable.SimpleExpression
import cspom.util.Infinitable
import cspom.util.Finite
import cspom.compiler.Delta

object CountEq extends ConstraintCompilerNoData {
  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = {
    c.function == 'count_eq
  }

  def selfPropagation: Boolean = false

  def compile(constraint: cspom.CSPOMConstraint[_], problem: cspom.CSPOM): cspom.compiler.Delta = {
    val Seq(CSPOMSeq(args), value, count: SimpleExpression[Int]) = constraint.arguments
    val ncount = reduceDomain(count, Interval[Infinitable](Finite(0), Finite(args.size)))

    problem.removeConstraint(constraint)
    val nconstraint = problem.ctr(CSPOMConstraint(ncount, 'occurrence, Seq(value, CSPOMSeq(args: _*)), constraint.params))

    Delta().removed(constraint) ++ Delta().added(nconstraint) ++ replace(Seq(count), ncount, problem)

    //replaceCtr(constraint, , problem)
  }
}