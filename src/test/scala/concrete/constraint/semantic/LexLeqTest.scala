package concrete.constraint.semantic


import concrete.Problem
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.BooleanDomain
import concrete.Assignment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class LexLeqTest extends AnyFlatSpec with Matchers {
  "lexleq" should "update alpha" in {
    val x = Array(
      new Variable("x0", BooleanDomain()),
      new Variable("x1", BooleanDomain()))

    val y = Array(
      new Variable("y0", BooleanDomain()),
      new Variable("y1", BooleanDomain()))

    val problem = new Problem(x ++ y)

    val c = new LexLeq(x, y)
    c.register(new AdviseCount)
    problem.addConstraint(c)

    val ps = problem.initState.toState

    ps(c) shouldBe ((0, 3))

    val ps2 = ps.assign(x(0), 1).toState
    c.event(ps2, Assignment, 0)
    val ps3 = c.revise(ps2).toState

    ps3(c) shouldBe ((1, 3))
    ps3.dom(y(0)) shouldBe BooleanDomain.TRUE

  }
}