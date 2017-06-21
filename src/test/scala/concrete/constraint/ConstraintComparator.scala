package concrete.constraint

import org.scalatest.Inspectors
import org.scalatest.Matchers

import concrete.Contradiction
import concrete.Problem
import concrete.ProblemState
import concrete.Variable
import concrete.Domain

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

    c1.adviseAll(ps)
    c2.adviseAll(ps)

    val r1 = c1.revise(ps)
    val r2 = c2.revise(ps)

//    r1
//      .andThen { r1 =>
//      println(s"${c1.toString(ps)} -> ${c1.toString(r1)}")
//      r1
//    }

    (r1, r2) match {
      case (_: Contradiction, _: Contradiction) =>
      case (s1: ProblemState, s2: ProblemState) =>
        forAll(s1.domains.zip(s2.domains)) {
          case t: Tuple2[Domain, Domain] => val d1 = t._1; val d2 = t._2; d1.view should contain theSameElementsAs d2.view
        }
        assert(!s2.entailed.hasInactiveVar(c2) || s1.entailed.hasInactiveVar(c1))
      case (s1, s2) => fail(s"$s1 and $s2 were not the same")
    }

  }

}