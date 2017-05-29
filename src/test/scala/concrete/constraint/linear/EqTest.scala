package concrete.constraint.linear

import concrete._
import cspom.CSPOM
import cspom.variable.IntVariable
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Inspectors, Matchers}

final class EqTest extends FlatSpec with Matchers with PropertyChecks with Inspectors {

  "EqBC" should "filter" in {
    val v0 = new Variable("v0", IntDomain.ofInterval(2, 3))
    val v1 = new Variable("v1", IntDomain.ofSeq(0, 2, 3))
    val c = new EqBC(false, v0, -1, v1)
    val pb = Problem(v0, v1)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.revise(ps)

    mod.dom(v0).view should contain theSameElementsAs Seq(3)
    mod.dom(v1).view should contain theSameElementsAs Seq(2)
  }

  "EqACFast" should "filter" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(1, 2, 3))
    val v1 = new Variable("v1", IntDomain.ofSeq(4, 5, 7))
    val c = new EqACFast(v0, 1, v1)
    // c.register(new AdviseCount())
    val pb = Problem(v0, v1)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.revise(ps)

    mod.dom(v0).view should contain theSameElementsAs Seq(3)
    mod.dom(v1).view should contain theSameElementsAs Seq(4)
  }

  it should "detect consistency correctly" in {
    val v0 = new Variable("v0", IntDomain(0 to 10))
    val v1 = new Variable("v1", IntDomain(10 to 20))
    forAll { b: Int =>
      val c = new EqACFast(v0, b, v1)
      val pb = Problem(v0, v1)
      pb.addConstraint(c)
      val ps = pb.initState.toState
      c.adviseAll(ps)

      val r = c.consistent(ps)
      if (0 < b && b < 20) {
        assert(r.isState)
      } else {
        assert(!r.isState)
      }

    }
  }

  it should "be reified correctly" in {
    import concrete.CSPOMDriver._
    val cspom: CSPOM = CSPOM { implicit pb: CSPOM =>
      val v0 = IntVariable(0 to 0) as "v0"
      val v1 = IntVariable(0 to 10) as "v1"
      linearReif("eq", 1, (1, v0), (-1, v1)) as "result"
    }

    val solver = Solver(cspom).get

    //  println(solver.solver.problem)

    val solutions = solver.toSeq

    solutions should have size 11

    for (s <- solutions) {
      s("result") shouldBe (false)
    }

  }
}