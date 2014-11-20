package concrete.constraint.semantic;

import scala.util.Random
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import concrete.IntDomain
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.Revised
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.prop.PropertyChecks

final class AbsDiffTest extends FlatSpec with Matchers with Inspectors with PropertyChecks {

  "AbsDiff" should "behave the same as extensional constraint" in {
    forAll { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val RAND = new Random
      val vx: Variable = new Variable("x", IntDomain(x: _*));
      val vy: Variable = new Variable("y", IntDomain(y: _*));
      val vz: Variable = new Variable("z", IntDomain(z: _*));
      val domains = IndexedSeq(vx.initDomain, vy.initDomain, vz.initDomain)

      val c = new AbsDiffAC(vx, vy, vz);
      c.register(new AdviseCount)
      c.adviseAll(domains)
      val Revised(doms1, _, _) = c.revise(domains, c.initState)

      val c2 = new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
        def check(t: Array[Int]) = t(0) == math.abs(t(1) - t(2));
      };
      c2.register(new AdviseCount)
      c2.adviseAll(doms1)
      val Revised(doms2, _, _) = c2.revise(doms1, c2.initState)

      forAll((doms1 zip doms2)) {
        case (d1, d2) => d1 should be theSameInstanceAs d2
      }
    }

    {
      val vx = new Variable("x", IntDomain(0, 2, 3))
      val vy = new Variable("y", IntDomain(3, 4, 6, 7, 8))
      val vz = new Variable("z", IntDomain(5))
      val domains = IndexedSeq(vx.initDomain, vy.initDomain, vz.initDomain)

      val c = new AbsDiffAC(vx, vy, vz)
      c.register(new AdviseCount)
      c.adviseAll(domains)
      val Revised(doms, _, _) = c.revise(domains, c.initState)

      doms(0) should have size 2
      doms(1) should have size 3
      doms(2) should have size 1
    }
  }

}
