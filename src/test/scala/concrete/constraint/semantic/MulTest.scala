package concrete.constraint.semantic;

import scala.IndexedSeq
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import concrete.IntDomain
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.Problem

final class MulTest extends FlatSpec with Matchers with PropertyChecks with Inspectors {

  val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  "Mul" should "filter the same as enumerators" in {
    //    {
    //      val vx = new Variable("x", IntDomain(0, -28))
    //      val vy = new Variable("y", IntDomain(-12, 15))
    //      val vz = new Variable("z", IntDomain(16))
    //
    //      val domains = IndexedSeq(vx.initDomain, vy.initDomain, vz.initDomain)
    //      val c = new MulAC(vx, vy, vz)
    //      c.register(new AdviseCount)
    //      c.adviseAll(domains)
    //
    //      val r1 = c.revise(domains)
    //
    //      //val Revised(mod, _, _) = c.revise(domains)
    //
    //      val c2 = new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
    //        def check(t: Array[Int]) = t(0) == t(1) * t(2);
    //      };
    //      c2.register(new AdviseCount)
    //      c2.adviseAll(domains)
    //      val r2 = c2.revise(domains)
    //      r1 shouldBe r2
    //    }

    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable("y", IntDomain.ofSeq(y: _*))
      val vz = new Variable("z", IntDomain.ofSeq(z: _*))

      val ps = Problem(vx, vy, vz).initState
      val c = new MulAC(vx, vy, vz)
      c.register(new AdviseCount)
      c.adviseAll(ps)
      val r1 = c.revise(ps)

      //val Revised(mod, _, _) = c.revise(domains)

      val c2 = new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
        def check(t: Array[Int]) = t(0) == t(1) * t(2);
      };
      c2.register(new AdviseCount)
      c2.adviseAll(ps)
      val r2 = c2.revise(ps)
      r1 shouldBe r2

      //      
      //      val Revised(mod2, _, _) = c2.revise(mod)
      //      forAll(mod zip mod2) { case (m1, m2) => m1 should be theSameInstanceAs m2 }
      //      println(s"$domains $mod $mod2")
    }
  }
}
