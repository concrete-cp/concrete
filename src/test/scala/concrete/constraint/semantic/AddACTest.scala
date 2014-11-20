package concrete.constraint.semantic

import concrete.IntDomain
import concrete.Problem
import concrete.Revised
import concrete.Variable
import concrete.constraint.AdviseCount
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.constraint.Residues
import org.scalatest.prop.PropertyChecks
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator

final class AddACTest extends FlatSpec with Matchers with PropertyChecks {

  "AddAC" should "filter X" in {
    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain(5))
    val z = new Variable("z", IntDomain(2))

    val c = new AddAC(x, y, z)

    val domains = IndexedSeq(x, y, z).map(_.initDomain)

    c.register(new AdviseCount)
    c.adviseAll(domains)
    assert(c.intervalsOnly(domains))
    val Revised(mod, _, _) = c.revise(domains)

    mod(0) should not be theSameInstanceAs(domains(0))
    mod(1) should be theSameInstanceAs domains(1)
    mod(2) should be theSameInstanceAs domains(2)

    mod(0) shouldBe Seq(7)

    assert(c.intervalsOnly(mod))

  }

  it should "filter Y" in {
    val x = new Variable("x", IntDomain(7))
    val y = new Variable("y", IntDomain(-100 to 100))
    val z = new Variable("z", IntDomain(2))

    val c = new AddAC(x, y, z)
    val domains = IndexedSeq(x, y, z).map(_.initDomain)

    c.register(new AdviseCount)
    c.adviseAll(domains)
    assert(c.intervalsOnly(domains))
    val Revised(mod, _, _) = c.revise(domains)
    mod(0) should be theSameInstanceAs (domains(0))
    mod(1) should not be theSameInstanceAs(domains(1))
    mod(2) should be theSameInstanceAs domains(2)

    mod(1) shouldBe Seq(5)

    assert(c.intervalsOnly(mod))
  }

  it should "filter Z" in {

    val x = new Variable("x", IntDomain(1 to 10))
    val y = new Variable("y", IntDomain(20 to 30))
    val z = new Variable("z", IntDomain(-100 to 100))

    val c = new AddAC(x, y, z)
    val domains = IndexedSeq(x, y, z).map(_.initDomain)

    c.register(new AdviseCount)
    c.adviseAll(domains)
    assert(c.intervalsOnly(domains))
    val Revised(mod, _, _) = c.revise(domains)
    mod(0) should be theSameInstanceAs (domains(0))
    mod(1) should be theSameInstanceAs (domains(1))
    mod(2) should not be theSameInstanceAs(domains(2))

    mod(2) shouldBe (-29 to -10)
    assert(c.intervalsOnly(mod))
  }

  it should "filter the same as enumerator" in {
    forAll { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val vx = new Variable("x", IntDomain(x: _*))
      val vy = new Variable("y", IntDomain(y: _*))
      val vz = new Variable("z", IntDomain(z: _*))

      val c = new AddAC(vx, vy, vz);

      c.register(new AdviseCount())
      val d = c.scope.map(_.initDomain)
      c.adviseAll(d)
      val Revised(d2, _, _) = c.revise(d, c.initState)

      val c2 = new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
        def check(t: Array[Int]) = t(0) == t(1) + t(2);
      };
      c2.register(new AdviseCount())
      c2.adviseAll(d2)
      val Revised(d3, _, _) = c2.revise(d2)
      assert((d2, d3).zipped.forall(_ eq _))
    }
  }
}