package concrete.constraint.semantic;

import scala.IndexedSeq
import scala.util.Random

import org.scalacheck.Gen
import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

import concrete.IntDomain
import concrete.Problem
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator

final class AbsDiffTest extends FlatSpec with Matchers with Inspectors with PropertyChecks {

  val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  "AbsDiff" should "behave the same as extensional constraint" in {
    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val RAND = new Random
      val vx: Variable = new Variable("x", IntDomain.ofSeq(x: _*));
      val vy: Variable = new Variable("y", IntDomain.ofSeq(y: _*));
      val vz: Variable = new Variable("z", IntDomain.ofSeq(z: _*));

      val c = new AbsDiffAC(vx, vy, vz);

      val pb = Problem(vx, vy, vz)
      pb.addConstraint(c)
      val ps = pb.initState.toState

      c.register(new AdviseCount)
      c.adviseAll(ps)
      val r1 = c.revise(ps)

      val c2 = new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
        def check(t: Array[Int]) = t(0) == math.abs(t(1) - t(2));
      };
      c2.register(new AdviseCount)
      c2.adviseAll(ps)
      val r2 = c2.revise(ps)

      r1 shouldBe r2
    }
  }

  it should "filter correctly" in {

    val vx = new Variable("x", IntDomain.ofSeq(0, 2, 3))
    val vy = new Variable("y", IntDomain.ofSeq(3, 4, 6, 7, 8))
    val vz = new Variable("z", IntDomain.ofSeq(5))
    val c = new AbsDiffAC(vx, vy, vz);

    val pb = Problem(vx, vy, vz)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.register(new AdviseCount)
    c.adviseAll(ps)
    val mod = c.revise(ps).asInstanceOf[ProblemState]

    mod.dom(vx) should have size 2
    mod.dom(vy) should have size 3
    mod.dom(vz) should have size 1

  }

  "AbsDiffBC" should "filter correctly" in {
    val vx = new Variable("x", IntDomain(5 to 7))
    val vy = new Variable("y", IntDomain(4 to 5))
    val vz = new Variable("z", IntDomain.ofSeq(9))

    val c = new AbsDiffBC(vx, vy, vz)
    val ps = Problem(vx, vy, vz).initState.toState
    val mod = c.revise(ps).asInstanceOf[ProblemState]

    mod.domains shouldBe IndexedSeq(IntDomain.ofSeq(5), IntDomain.ofSeq(4), IntDomain.ofSeq(9))
  }

}
