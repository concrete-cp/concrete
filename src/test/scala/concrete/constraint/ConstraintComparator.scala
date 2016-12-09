package concrete.constraint

import org.scalatest.Inspectors
import org.scalatest.Matchers

import concrete.Contradiction
import concrete.Domain
import concrete.Problem
import concrete.ProblemState
import concrete.Variable

/**
 * @author vion
 */
object ConstraintComparator extends Matchers with Inspectors {

  def compare(vars: Array[Variable], c1: Constraint, c2: Constraint): Unit = {
    val problem = new Problem(vars)
    problem.addConstraint(c1)
    problem.addConstraint(c2)
    val ac = new AdviseCount
    Seq(c1, c2)
      .collect {
        case c: Advisable => c
      }
      .foreach(_.register(ac))

    val ps = problem.initState.toState

    val r1 = c1.revise(ps)
    val r2 = c2.revise(ps)

    (r1, r2) match {
      case (_: Contradiction, _: Contradiction) =>
      case (s1: ProblemState, s2: ProblemState) =>
        forAll(s1.domains zip s2.domains) {
          case (d1: Domain, d2: Domain) => d1.view should contain theSameElementsAs d2.view
        }
        assert(!s2.isEntailed(c2) || s1.isEntailed(c1))
      case _ => fail()
    }

  }

}