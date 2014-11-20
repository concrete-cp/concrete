package concrete.constraint.semantic;

import scala.IndexedSeq

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

import concrete.IntDomain
import concrete.Revised
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator

final class MulTest extends FlatSpec with Matchers with PropertyChecks with Inspectors {

  "Mul" should "filter the same as enumerators" in {
    forAll { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val vx = new Variable("x", IntDomain(x: _*))
      val vy = new Variable("y", IntDomain(y: _*))
      val vz = new Variable("z", IntDomain(z: _*))

      val domains = IndexedSeq(vx.initDomain, vy.initDomain, vz.initDomain)
      val c = new MulAC(vx, vy, vz)
      c.register(new AdviseCount)
      c.adviseAll(domains)
      val Revised(mod, _, _) = c.revise(domains)

      val c2 = new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
        def check(t: Array[Int]) = t(0) == t(1) * t(2);
      };
      c2.register(new AdviseCount)
      c2.adviseAll(mod)
      val Revised(mod2, _, _) = c2.revise(mod)
      forAll(mod zip mod2) { case (m1, m2) => m1 should be theSameInstanceAs m2 }
    }
  }
}
