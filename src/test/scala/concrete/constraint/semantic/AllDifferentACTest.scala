package concrete.constraint.semantic

import concrete.constraint.AdviseCount
import concrete.{IntDomain, Problem, Variable}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AllDifferentACTest extends AnyFlatSpec with Matchers {

  "AllDifferentAC" should "detect contradiction" in {
    val v1 = new Variable("1", IntDomain.ofSeq(7))
    val v2 = new Variable("2", IntDomain.ofSeq(6))
    val v3 = new Variable("3", IntDomain.ofSeq(7, 9))
    val v4 = new Variable("4", IntDomain.ofSeq(8))
    val v5 = new Variable("5", IntDomain.ofSeq(8, 9))

    val problem = Problem(v1, v2, v3, v4, v5)
    val c = new AllDifferentAC(v1, v2, v3, v4, v5)
    c.register(new AdviseCount)
    problem.addConstraint(c)

    val mod = problem.initState.andThen { ps =>
      c.eventAll(ps)
      c.revise(ps)
    }
    assert(!mod.isState)
  }

  it should "filter" in {
    val v1 = new Variable("v1", IntDomain.ofSeq(3))
    val v2 = new Variable("v2", IntDomain(0 to 5))
    val v3 = new Variable("v3", IntDomain.ofSeq(0, 1, 5))
    val v4 = new Variable("v4", IntDomain.ofSeq(0, 5))
    val v5 = new Variable("v5", IntDomain.ofSeq(0, 1, 5))

    val problem = Problem(v1, v2, v3, v4, v5)
    val c = new AllDifferentAC(v1, v2, v3, v4, v5)
    c.register(new AdviseCount)
    problem.addConstraint(c)

    val mod = problem.initState.andThen { ps =>
      c.eventAll(ps)
      c.revise(ps)
    }.toState

    mod.dom(v2) should contain theSameElementsAs Seq(2, 4)

    c.eventAll(mod)
    assert(c.revise(mod) == mod)
  }

}