package concrete.constraint.semantic

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Problem
import concrete.constraint.AdviseCount
import concrete.constraint.Advisable
import concrete.Domain
import concrete.Contradiction
import concrete.ProblemState
import org.scalatest.Inspectors

/**
 * @author vion
 */
object ConstraintComparator extends FlatSpec with Matchers with Inspectors {

  def compare(vars: List[Variable], c1: Constraint, c2: Constraint): Unit = {
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
      case (Contradiction, Contradiction) =>
      case (s1: ProblemState, s2: ProblemState) =>
        forAll(s1.domains zip s2.domains) {
          case (d1: Domain, d2: Domain) => d1 should contain theSameElementsAs d2
        }
        assert(!s2.isEntailed(c2) || s1.isEntailed(c1))
      case _ => fail()
    }

  }

}