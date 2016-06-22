package concrete.constraint.linear

import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import concrete.IntDomain
import concrete.Problem
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.constraint.ConstraintComparator

final class AddACTest extends FlatSpec with Matchers with PropertyChecks {

  "AddAC" should "filter X" in {
    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain.ofSeq(5))
    val z = new Variable("z", IntDomain.ofSeq(2))

    val c = new SumAC(0, Array(-1, 1, 1), Array(x, y, z), SumEQ)

    val pb = Problem(x, y, z)
    val ps = pb.initState.toState
    pb.addConstraint(c)

    c.register(new AdviseCount)
    c.adviseAll(ps)
    assert(c.intervalsOnly(ps))
    val mod = c.revise(ps).toState

    mod.dom(x) should not be theSameInstanceAs(ps.dom(x))
    mod.dom(y) should be theSameInstanceAs ps.dom(y)
    mod.dom(z) should be theSameInstanceAs ps.dom(z)

    mod.dom(x).view should contain theSameElementsAs Seq(7)

    assert(c.intervalsOnly(mod))

  }

  it should "filter Y" in {
    val x = new Variable("x", IntDomain.ofSeq(7))
    val y = new Variable("y", IntDomain(-100 to 100))
    val z = new Variable("z", IntDomain.ofSeq(2))

    val c = new SumAC(0, Array(-1, 1, 1), Array(x, y, z), SumEQ)
    val pb = Problem(x, y, z)
    val ps = pb.initState.toState
    pb.addConstraint(c)

    c.register(new AdviseCount)
    c.adviseAll(ps)
    assert(c.intervalsOnly(ps))
    val mod = c.revise(ps).toState
    mod.dom(x) should be theSameInstanceAs (ps.dom(x))
    mod.dom(y) should not be theSameInstanceAs(ps.dom(y))
    mod.dom(z) should be theSameInstanceAs ps.dom(z)

    mod.dom(y).view should contain theSameElementsAs Seq(5)

    assert(c.intervalsOnly(mod))
  }

  it should "filter Z" in {

    val x = new Variable("x", IntDomain(1 to 10))
    val y = new Variable("y", IntDomain(20 to 30))
    val z = new Variable("z", IntDomain(-100 to 100))
    val c = new SumAC(0, Array(-1, 1, 1), Array(x, y, z), SumEQ)
    val pb = Problem(x, y, z)
    val ps = pb.initState.toState
    pb.addConstraint(c)

    c.register(new AdviseCount)
    c.adviseAll(ps)
    assert(c.intervalsOnly(ps))
    val mod = c.revise(ps)
    mod.dom(x) should be theSameInstanceAs (ps.dom(x))
    mod.dom(y) should be theSameInstanceAs (ps.dom(y))

    mod.dom(z) should not be theSameInstanceAs(ps.dom(z))
    mod.dom(z).view should contain theSameElementsAs (-29 to -10)

    assert(c.intervalsOnly(ps))
  }

  val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  it should "filter the same as enumerator" in {
    {
      val vx = new Variable("x", IntDomain.ofSeq(0, 0))
      val vy = new Variable("y", IntDomain.ofSeq(0))
      val vz = new Variable("z", IntDomain.ofSeq(0))

      ConstraintComparator.compare(
        Array(vx, vy, vz),
        new SumAC(0, Array(-1, 1, 1), Array(vx, vy, vz), SumEQ),
        new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
          def check(t: Array[Int]) = t(0) == t(1) + t(2);
        })

    }

    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable("y", IntDomain.ofSeq(y: _*))
      val vz = new Variable("z", IntDomain.ofSeq(z: _*))

      ConstraintComparator.compare(
        Array(vx, vy, vz),
        new SumAC(0, Array(-1, 1, 1), Array(vx, vy, vz), SumEQ),
        new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
          def check(t: Array[Int]) = t(0) == t(1) + t(2);
        })

    }

  }
}