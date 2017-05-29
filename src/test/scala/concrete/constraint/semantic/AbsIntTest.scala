package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.IntDomain
import concrete.Problem
import concrete.ProblemState
import concrete.Variable
import org.scalatest.time.Second
import org.scalatest.time.Span
import org.scalatest.prop.PropertyChecks
import concrete.constraint.ResiduesRemovals
import org.scalacheck.Gen
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.ConstraintComparator
import org.scalatest.concurrent.TimeLimits

final class AbsIntTest extends FlatSpec with Matchers with TimeLimits with PropertyChecks {

  "AbsInt" should "filter X" in {

    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain.ofSeq(-5))

    val c = new AbsBC(x, y)

    val pb = Problem(x, y)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)

    assert(c.intervalsOnly(ps))

    val mod: ProblemState = c.revise(ps).toState

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
    val ps = pb.initState.toState
    c.adviseAll(ps)
    assert(c.intervalsOnly(ps))
    val mod = c.revise(ps).toState

    mod.dom(x) should be theSameInstanceAs ps.dom(x)
    mod.dom(y) should not be theSameInstanceAs(ps.dom(y))

    mod.dom(y).view should contain theSameElementsAs Seq(-7, 7)

    assert(!c.intervalsOnly(mod))
    assert(mod.dom(x).convex)
  }

  it should "not hang" in {
    val x = new Variable("x", IntDomain.ofSeq(57, 224))
    val y = new Variable("y", IntDomain.ofSeq(-224))

    val c = new AbsBC(x, y)
    val pb = Problem(x, y)
    pb.addConstraint(c)

    failAfter(Span(1, Second)) {
      val ps = pb.initState.toState

      val mod = c.revise(ps).toState

      mod.dom(x).view should contain theSameElementsAs Seq(224)
      mod.dom(y) should be theSameInstanceAs ps.dom(y)
    }

  }

  val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  it should "filter the same as enumerator" in {

    forAll(dom, dom) { (x: Seq[Int], y: Seq[Int]) =>
      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable("y", IntDomain.ofSeq(y: _*))

      ConstraintComparator.compare(
        Array(vx, vy),
        new AbsAC(vx, vy),
        new Constraint(Array(vx, vy)) with ResiduesRemovals with TupleEnumerator {
          def check(t: Array[Int]) = t(0) == math.abs(t(1))
        })

    }

  }

}