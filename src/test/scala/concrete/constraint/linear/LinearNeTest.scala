package concrete.constraint.linear

import scala.annotation.varargs

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.BooleanDomain
import concrete.IntDomain
import concrete.Problem
import concrete.Variable

class LinearNeTest extends FlatSpec with Matchers {
  "LinearNe" should "not filter" in {

    val x_2095 = new Variable("X2095", BooleanDomain.UNKNOWNBoolean)
    val x_2055 = new Variable("X2055", BooleanDomain.UNKNOWNBoolean)

    val ctr = new LinearNe2(2, Array(2, 1), Array(x_2095, x_2055))

    val pb = Problem(x_2095, x_2055)
    pb.addConstraint(ctr)

    val res = pb.initState
      .andThen { ps => ctr.revise(ps) }
      .andThen(_.assign(x_2055, 1))
      .andThen { ps => ctr.revise(ps) }
      .toState

    res.dom(x_2095) shouldBe BooleanDomain.UNKNOWNBoolean

  }

  it should "filter" in {

    val x_2095 = new Variable("X2095", BooleanDomain.UNKNOWNBoolean)
    val x_2055 = new Variable("X2055", BooleanDomain.UNKNOWNBoolean)

    val ctr = new LinearNe2(2, Array(2, 1), Array(x_2095, x_2055))

    val pb = Problem(x_2095, x_2055)
    pb.addConstraint(ctr)

    val res = pb.initState
      .andThen { ps => ctr.revise(ps) }
      .andThen(_.assign(x_2055, 0))
      .andThen { ps => ctr.revise(ps) }
      .toState

    res.dom(x_2095) shouldBe BooleanDomain.FALSE

  }
}
