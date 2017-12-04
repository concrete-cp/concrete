package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.IntDomain
import concrete.Variable
import concrete.Problem
import concrete.constraint.AdviseCount

class AllDifferentBCTest extends FlatSpec with Matchers {

  "AllDifferentBC" should "detect contradiction" in {
    val v1 = new Variable("1", IntDomain.ofSeq(7))
    val v2 = new Variable("2", IntDomain.ofSeq(6))
    val v3 = new Variable("3", IntDomain.ofSeq(7, 9))
    val v4 = new Variable("4", IntDomain.ofSeq(8))
    val v5 = new Variable("5", IntDomain.ofSeq(8, 9))

    val problem = Problem(v1, v2, v3, v4, v5)
    val c = new AllDifferentBC(v1, v2, v3, v4, v5)
    c.register(new AdviseCount)
    problem.addConstraint(c)

    val mod = problem.initState.andThen { ps =>
      c.eventAll(ps)
      c.revise(ps)
    }
    assert(!mod.isState)
  }

}