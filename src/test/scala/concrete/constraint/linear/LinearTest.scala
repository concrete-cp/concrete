package concrete.constraint.linear

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.IntDomain
import concrete.Variable
import concrete.ParameterManager
import concrete.Problem
import concrete.constraint.AdviseCount
import concrete.Assignment

class LinearTest extends FlatSpec with Matchers {
  "LinearLe" should "correctly update domain cache" in {
    val pm = new ParameterManager
    val ranges = Seq(
      6 to 118, 0 to 1, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 1 to 1, 1 to 1, 1 to 1, 1 to 1, 1 to 1, 1 to 1, 0 to 1, 0 to 1, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 1, 0 to 1, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 1, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0, 0 to 0)
    val variables = ranges.zipWithIndex.map { case (r, i) => new Variable(s"v$i", IntDomain(r)) }.toArray

    val constraint = LinearLe(0, Array(1).padTo(variables.length, -1), variables, false, pm)
    constraint.register(new AdviseCount)

    val problem = new Problem(variables)

    problem.addConstraint(constraint)

    val s1 = problem.initState.toState
    val m1 = constraint.revise(s1).toState

    //println(constraint.toString(m1))

    val m2 = m1.assign(variables(0), 6)
      .toState

    for (i <- 0 to 0; p <- constraint.position(variables(i))) {
      constraint.advise(m2, Assignment, p)
    }

    val m3 = constraint.revise(m2).toState

    //println(constraint.toString(m3))
    
    //println(m3.entailed.hasInactiveVar(constraint))

  }
}