package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.Problem
import concrete.Variable
import concrete.BooleanDomain
import concrete.constraint.AdviseCount

class LexLeqTest extends FlatSpec with Matchers {
  "lexleq" should "update alpha" in {
    val x = Array(
      new Variable("x0", BooleanDomain()),
      new Variable("x1", BooleanDomain()))

    val y = Array(
      x(0),
      new Variable("y1", BooleanDomain()))

    val problem = new Problem(x.toList :+ y(1))

    val c = new LexLeq(x, y)
    c.register(new AdviseCount)
    problem.addConstraint(c)

    val ps = problem.initState.toState
    println(c.toString(ps))

    val ps2 = ps.assign(x(0), 1).toState
    c.advise(ps2, Seq(0, 2))
    val ps3 = c.revise(ps2).toState
    println(c.toString(ps3))
    


  }
}