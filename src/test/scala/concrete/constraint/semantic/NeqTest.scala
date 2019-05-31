package concrete.constraint.semantic

import concrete.{IntDomain, Problem, Singleton, Variable}
import concrete.constraint.AdviseCount
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

final class NeqTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks {

  "Neq" should "not filter" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(1, 2, 3))
    val v1 = new Variable("v1", IntDomain.ofSeq(3, 4, 5))

    forAll { k: Int =>
      val c = new Neq(v0, v1, k)
      val pb = Problem(v0, v1)
      pb.addConstraint(c)
      c.register(new AdviseCount)
      val ps = pb.initState
      val revised = ps.andThen { ps =>
        c.eventAll(ps)
        c.revise(ps)
      }
      revised.toState.domains shouldBe ps.toState.domains

      whenever(k > 0 || k < -4) {
        assert(revised.toState.entailed.hasInactiveVar(c))
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
      c.register(new AdviseCount)

      val ps = pb.initState
      val mod = ps.andThen { ps =>
        c.eventAll(ps)
        c.revise(ps)
      }

      (ps.dom(v0).view.toSet -- mod.dom(v0).view) should contain theSameElementsAs Seq(-5 + k)
    }

    {
      val pb = Problem(v0, v1)
      val c = new Neq(v1, v0, k)
      pb.addConstraint(c)
      c.register(new AdviseCount)
      val ps = pb.initState
      val mod = ps.andThen { ps =>
        c.eventAll(ps)
        c.revise(ps)
      }

      (ps.dom(v0).view.toSet -- mod.dom(v0).view) should contain theSameElementsAs Seq(-5 - k)
    }

    {
      val pb = Problem(v0, v1)
      val c = new Neq(v0, v1, -100)
      pb.addConstraint(c)
      c.register(new AdviseCount)
      val ps = pb.initState
      val mod = ps.andThen { ps =>
        c.eventAll(ps)
        c.revise(ps)
      }

      mod.dom(v0) should be theSameInstanceAs ps.dom(v0)
      assert(mod.toState.entailed.hasInactiveVar(c))
    }


  }

}