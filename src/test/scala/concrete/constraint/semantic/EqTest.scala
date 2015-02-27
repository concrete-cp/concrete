package concrete.constraint.semantic

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.IntDomain
import concrete.Problem
import concrete.Variable
import concrete.constraint.AdviseCount

final class EqTest extends FlatSpec with Matchers {

  "Eq" should "filter" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(1, 2, 3))
    val v1 = new Variable("v1", IntDomain.ofSeq(3, 4, 5))
    val c = new EqAC(v0, v1)
    c.register(new AdviseCount())
    val pb = Problem(v0,v1)
    pb.addConstraint(c)
    val ps = pb.initState
    c.adviseAll(ps)
    val mod = c.revise(ps)

    mod.dom(v0) shouldBe Seq(3)
    mod.dom(v1) shouldBe Seq(3)
  }
}