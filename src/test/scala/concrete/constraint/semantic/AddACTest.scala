package concrete.constraint.semantic

import org.scalacheck.Gen
import org.scalatest.FlatSpec
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

final class AddACTest extends FlatSpec with Matchers with PropertyChecks {

  "AddAC" should "filter X" in {
    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain.ofSeq(5))
    val z = new Variable("z", IntDomain.ofSeq(2))

    val c = new SumAC(0, Array(-1, 1, 1), Array(x, y, z), SumMode.SumEQ)

    val pb = Problem(x, y, z)
    val ps = pb.initState.toState
    pb.addConstraint(c)

    c.register(new AdviseCount)
    c.adviseAll(ps)
    assert(c.intervalsOnly(ps))
    val mod = c.consistentRevise(ps)

    mod.dom(x) should not be theSameInstanceAs(ps.dom(x))
    mod.dom(y) should be theSameInstanceAs ps.dom(y)
    mod.dom(z) should be theSameInstanceAs ps.dom(z)

    mod.dom(x) shouldBe Seq(7)

    assert(c.intervalsOnly(mod))

  }

  it should "filter Y" in {
    val x = new Variable("x", IntDomain.ofSeq(7))
    val y = new Variable("y", IntDomain(-100 to 100))
    val z = new Variable("z", IntDomain.ofSeq(2))

    val c = new SumAC(0, Array(-1, 1, 1), Array(x, y, z), SumMode.SumEQ)
    val pb = Problem(x, y, z)
    val ps = pb.initState.toState
    pb.addConstraint(c)

    c.register(new AdviseCount)
    c.adviseAll(ps)
    assert(c.intervalsOnly(ps))
    val mod = c.consistentRevise(ps)
    mod.dom(x) should be theSameInstanceAs (ps.dom(x))
    mod.dom(y) should not be theSameInstanceAs(ps.dom(y))
    mod.dom(z) should be theSameInstanceAs ps.dom(z)

    mod.dom(y) shouldBe Seq(5)

    assert(c.intervalsOnly(mod))
  }

  it should "filter Z" in {

    val x = new Variable("x", IntDomain(1 to 10))
    val y = new Variable("y", IntDomain(20 to 30))
    val z = new Variable("z", IntDomain(-100 to 100))
    val c = new SumAC(0, Array(-1, 1, 1), Array(x, y, z), SumMode.SumEQ)
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

    mod.dom(z) shouldBe (-29 to -10)
    assert(c.intervalsOnly(ps))
  }

  val dom = Gen.nonEmptyListOf(Gen.choose(-1000, 1000))

  it should "filter the same as enumerator" in {
    {
      val vx = new Variable("x", IntDomain.ofSeq(0))
      val vy = new Variable("y", IntDomain.ofSeq(0))
      val vz = new Variable("z", IntDomain.ofSeq(-1))
      val c = new SumAC(0, Array(-1, 1, 1), Array(vx, vy, vz), SumMode.SumEQ)
      val pb = Problem(vx, vy, vz)
      val ps = pb.initState.toState
      pb.addConstraint(c)
      c.register(new AdviseCount())
      c.adviseAll(ps)
      val r1 = c.revise(ps)

      val c2 = new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
        def check(t: Array[Int]) = t(0) == t(1) + t(2);
      };
      c2.register(new AdviseCount())
      c2.adviseAll(ps)
      val r2 = c2.revise(ps)

      r1 shouldBe r2
    }

    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      val vx = new Variable("x", IntDomain.ofSeq(x: _*))
      val vy = new Variable("y", IntDomain.ofSeq(y: _*))
      val vz = new Variable("z", IntDomain.ofSeq(z: _*))

      val c = new SumAC(0, Array(-1, 1, 1), Array(vx, vy, vz), SumMode.SumEQ)
      val pb = Problem(vx, vy, vz)
      val ps = pb.initState.toState
      pb.addConstraint(c)

      c.register(new AdviseCount())
      val d = c.scope.map(_.initDomain)
      c.adviseAll(ps)
      val r1 = c.revise(ps)

      val c2 = new Constraint(Array(vx, vy, vz)) with Residues with TupleEnumerator {
        def check(t: Array[Int]) = t(0) == t(1) + t(2);
      };

      pb.addConstraint(c2)
      c2.register(new AdviseCount())
      c2.adviseAll(ps)
      val r2 = c2.revise(ps)

      r1.domainsOption shouldBe r2.domainsOption

    }

  }
}