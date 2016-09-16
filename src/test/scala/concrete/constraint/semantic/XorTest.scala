package concrete.constraint.semantic

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.BooleanDomain
import concrete.Variable
import concrete.Problem
import concrete.Contradiction
import concrete.Assignment

class XorTest extends FlatSpec with Matchers {
  "Xor constraint" should "assign even variables" in {
    val b1 = new Variable("b1", BooleanDomain())
    val b2 = new Variable("b2", BooleanDomain())

    val prob = Problem(b1, b2)

    val c = new Xor(Array(b1, b2))
    prob.addConstraint(c)

    val state = prob.initState.toState

    c.adviseAll(state) shouldBe 1
    c.revise(state) should be theSameInstanceAs state

    val s2 = state.assign(b1, 1).toState
    c.advise(s2, Assignment, 0) shouldBe 1

    val s2p = c.revise(s2)
    s2p.dom(b2) shouldBe BooleanDomain.FALSE

    val s3 = s2.assign(b2, 1).toState
    c.advise(s3, Assignment, 1) shouldBe 1

    c.revise(s3) shouldBe Contradiction
  }
}