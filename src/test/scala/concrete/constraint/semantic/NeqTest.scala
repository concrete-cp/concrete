package concrete.constraint.semantic

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.IntDomain
import concrete.Problem
import concrete.Variable
import concrete.constraint.AdviseCount
import org.scalatest.prop.PropertyChecks
import concrete.Singleton

final class NeqTest extends FlatSpec with Matchers with PropertyChecks {

  "Neq" should "not filter" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(1, 2, 3))
    val v1 = new Variable("v1", IntDomain.ofSeq(3, 4, 5))

    forAll { k: Int =>
      val c = new Neq(v0, v1, k)
      val pb = Problem(v0, v1)
      pb.addConstraint(c)
      val ps = pb.initState.toState
      c.adviseAll(ps)
      val revised = c.revise(ps)
      revised.domainsOption shouldBe ps.domainsOption

      whenever(k > 0 || k < -4) {
        assert(revised.isEntailed(c))
      }
      //
      //      mod.dom(v0) should contain theSameElementsAs Seq(3)
      //      mod.dom(v1) should contain theSameElementsAs Seq(3)

    }

  }

  it should "filter" in {
    val v0 = new Variable("v0", IntDomain.ofInterval(-100, 100))
    val v1 = new Variable("v1", Singleton(-5))

    val k = -5

    {
      val pb = Problem(v0, v1)
      val c = new Neq(v0, v1, k)
      pb.addConstraint(c)

      val ps = pb.initState.toState
      val mod = c.revise(ps)

      (ps.dom(v0).toSet -- mod.dom(v0)) should contain theSameElementsAs Seq(-5 + k)
      assert(mod.isEntailed(c))
    }

    {
      val pb = Problem(v0, v1)
      val c = new Neq(v1, v0, k)
      pb.addConstraint(c)

      val ps = pb.initState.toState
      val mod = c.revise(ps)

      (ps.dom(v0).toSet -- mod.dom(v0)) should contain theSameElementsAs Seq(-5 - k)
      assert(mod.isEntailed(c))
    }

  }

}