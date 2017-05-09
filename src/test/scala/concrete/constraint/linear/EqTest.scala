package concrete.constraint.linear

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.IntDomain
import concrete.Problem
import concrete.Variable

final class EqTest extends FlatSpec with Matchers {

  "EqBC" should "filter" in {
    val v0 = new Variable("v0", IntDomain.ofInterval(2, 3))
    val v1 = new Variable("v1", IntDomain.ofSeq(0, 2, 3))
    val c = new EqBC(false, v0, -1, v1)
    val pb = Problem(v0, v1)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.revise(ps)

    mod.dom(v0).view should contain theSameElementsAs Seq(3)
    mod.dom(v1).view should contain theSameElementsAs Seq(2)
  }

  "EqACFast" should "filter" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(1, 2, 3))
    val v1 = new Variable("v1", IntDomain.ofSeq(4, 5, 7))
    val c = new EqACFast(v0, 1, v1)
    // c.register(new AdviseCount())
    val pb = Problem(v0, v1)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.revise(ps)

    mod.dom(v0).view should contain theSameElementsAs Seq(3)
    mod.dom(v1).view should contain theSameElementsAs Seq(4)
  }
}