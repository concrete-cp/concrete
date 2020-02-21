package concrete
package constraint
package semantic


import org.scalacheck.Gen
import org.scalatest.Inspectors
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class AbsDiffTest extends AnyFlatSpec with Matchers with Inspectors with ScalaCheckPropertyChecks {

  private val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  "AbsDiff" should "behave the same as extensional constraint" in {
    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>

      val vx: Variable = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy: Variable = new Variable("y", IntDomain.ofSeq(y: _*))
      val vz: Variable = new Variable("z", IntDomain.ofSeq(z: _*))

      whenever (!vx.initDomain.convex || !vy.initDomain.convex || !vz.initDomain.convex) {
        ConstraintComparator.compare(
          Array(vx, vy, vz),
          new AbsDiffAC(vx, vy, vz),
          new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
            def check(t: Array[Int]): Boolean = t(0) == math.abs(t(1) - t(2))
          })
      }

    }
  }

  it should "filter correctly" in {

    val vx = new Variable("x", IntDomain.ofSeq(0, 2, 3))
    val vy = new Variable("y", IntDomain.ofSeq(3, 4, 6, 7, 8))
    val vz = new Variable("z", IntDomain.ofSeq(5))
    val c = new AbsDiffAC(vx, vy, vz)

    val pb = Problem(vx, vy, vz)
    pb.addConstraint(c)
    c.register(new AdviseCount)
    val mod = pb.initState.andThen { ps =>
      c.eventAll(ps)
      c.revise(ps)
    }

    mod.dom(vx) should have size 2
    mod.dom(vy) should have size 3
    mod.dom(vz) should have size 1

  }

  "AbsDiffBC" should "filter correctly" in {
    val vx = new Variable("x", IntDomain(5 to 7))
    val vy = new Variable("y", IntDomain(4 to 5))
    val vz = new Variable("z", IntDomain.ofSeq(9))

    val c = new AbsDiffBC(vx, vy, vz)
    val pb = Problem(vx, vy, vz)
    pb.addConstraint(c)
    c.register(new AdviseCount)
    val mod = pb.initState.andThen { ps =>
      c.eventAll(ps)
      c.revise(ps)
    }

    val Seq(dx, dy, dz) = Seq(vx, vy, vz).map(mod.dom)

    dx.view should contain theSameElementsAs Seq(5)
    dy.view should contain theSameElementsAs Seq(4)
    dz.view should contain theSameElementsAs Seq(9)

  }

}
