package concrete.constraint.semantic

import concrete.constraint._
import concrete.{Contradiction, IntDomain, Problem, Variable}
import org.scalacheck.Gen
import org.scalatest.concurrent.TimeLimits
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.{Second, Span}
import org.scalatest.{FlatSpec, Matchers}

final class AbsIntTest extends FlatSpec with Matchers with TimeLimits with PropertyChecks {

  "AbsInt" should "filter X" in {

    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain.ofSeq(-5))

    val c = new AbsBC(x, y)

    val pb = Problem(x, y)
    pb.addConstraint(c)

    c.register(new AdviseCount)

    val ps = pb.initState

    val mod = ps
      .andThen { ps =>
        c.eventAll(ps)
        assert(c.intervalsOnly(ps))
        c.revise(ps)
      }
      .toState

    mod.dom(x) should not be theSameInstanceAs(ps.dom(x))
    mod.dom(y) should be theSameInstanceAs ps.dom(y)

    mod.dom(x).view should contain theSameElementsAs Seq(5)

    assert(c.intervalsOnly(mod))

  }

  it should "filter Y" in {

    val x = new Variable("x", IntDomain.ofSeq(7))
    val y = new Variable("y", IntDomain(-100 to 100))

    val c = new AbsAC(x, y)

    val pb = Problem(x, y)
    pb.addConstraint(c)
    c.register(new AdviseCount)

    val ps = pb.initState

    val mod = ps.andThen { ps =>
      c.eventAll(ps)
      assert(c.intervalsOnly(ps))
      c.revise(ps)
    }

    mod.dom(x) should be theSameInstanceAs ps.dom(x)
    mod.dom(y) should not be theSameInstanceAs(ps.dom(y))

    mod.dom(y).view should contain theSameElementsAs Seq(-7, 7)

    assert(!c.intervalsOnly(mod.toState))
    assert(mod.dom(x).convex)
  }

  it should "not hang" in {
    val x = new Variable("x", IntDomain.ofSeq(57, 224))
    val y = new Variable("y", IntDomain.ofSeq(-224))

    val c = new AbsBC(x, y)
    val pb = Problem(x, y)
    pb.addConstraint(c)
    c.register(new AdviseCount)

    failAfter(Span(1, Second)) {
      val ps = pb.initState
      val mod = ps.andThen { ps =>
        c.eventAll(ps)
        c.revise(ps)
      }


      mod.dom(x).view should contain theSameElementsAs Seq(224)
      mod.dom(y) should be theSameInstanceAs ps.dom(y)
    }

  }

  it should "filter the same as enumerator, {0} = |{-1}|" in {


    val vx = new Variable("x", IntDomain.ofSeq(0))
    val vy = new Variable("y", IntDomain.ofSeq(-1))

    val problem = Problem(vx, vy)
    val c = new AbsAC(vx, vy)
    problem.addConstraint(c)
    c.register(new AdviseCount)

    problem.initState
      .andThen { ps =>
        c.eventAll(ps)
        c.revise(ps)
      } shouldBe a[Contradiction]


    ConstraintComparator.compare(
      Array(vx, vy),
      new AbsAC(vx, vy),
      new Constraint(Array(vx, vy)) with Residues with TupleEnumerator {
        def check(t: Array[Int]): Boolean = t(0) == math.abs(t(1))
      })


  }


  private val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  it should "filter the same as enumerator" in {

    forAll(dom, dom) { (x: Seq[Int], y: Seq[Int]) =>
      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable("y", IntDomain.ofSeq(y: _*))

      ConstraintComparator.compare(
        Array(vx, vy),
        new AbsAC(vx, vy, skipLarge = false),
        new Constraint(Array(vx, vy)) with Residues with TupleEnumerator {
          def check(t: Array[Int]): Boolean = t(0) == math.abs(t(1))
        })

    }

  }

}