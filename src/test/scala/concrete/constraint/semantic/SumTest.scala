package concrete.constraint.semantic;

import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers

import concrete.Contradiction
import concrete.IntDomain
import concrete.ParameterManager
import concrete.Problem
import concrete.ProblemState
import concrete.Singleton
import concrete.Solver
import concrete.Variable
import concrete.constraint.semantic.SumMode._
import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.IntVariable

final class SumTest extends FlatSpec with Matchers with Inspectors {

  import org.scalameter._
  val measure = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true)
    .withWarmer {
      new Warmer.Default
    }

  val pm = new ParameterManager()
  pm("linear.stateless") = true

  "Sum" should "filter <= with negative coefficients" in {
    val x = new Variable("x", IntDomain.ofSeq(0, 1))
    val y = new Variable("y", IntDomain.ofSeq(0, 1))
    val c = LinearLe(-19, Array(-20, -18), Array(x, y), false, pm)
    val pb = Problem(x, y)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)
    mod.dom(x) should contain theSameElementsAs Seq(1)
    mod.dom(y) should contain theSameElementsAs Seq(0, 1)

  }

  val v0 = new Variable("v0", IntDomain(1 to 4))
  val v1 = new Variable("v1", IntDomain(0 to 4))
  val b = new Variable("b", IntDomain.ofSeq(1))

  it should "filter =" in {
    val c = new LinearEq(0, Array(4, -1, -1), Array(b, v0, v1))
    val pb = Problem(b, v0, v1)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)

    mod.dom(v0) should contain theSameElementsAs Seq(1, 2, 3, 4)
    mod.dom(v1) should contain theSameElementsAs Seq(0, 1, 2, 3)
  }

  it should "filter <= with positive and negative coefficients" in {
    val c = LinearLe(0, Array(4, -1, -1), Array(b, v0, v1), false, pm)
    val pb = Problem(b, v0, v1)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)

    mod.dom(v0) should contain theSameElementsAs Seq(1, 2, 3, 4)
    mod.dom(v1) should contain theSameElementsAs (0 to 4)
  }

  it should "filter <= with positive coefficients" in {
    val c = LinearLe(3, Array(1, 1), Array(v0, v1), false, pm)
    val pb = Problem(b, v0, v1)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)
    mod.dom(v0) should contain theSameElementsAs (1 to 3)
    mod.dom(v1) should contain theSameElementsAs (0 to 2)
  }

  it should "filter <= with other negative coefficients and domains" in {

    val v2 = new Variable("v2", IntDomain(0 to 1))
    val c = LinearLe(-3, Array(-1, -1, -6), Array(
      new Variable("v0", IntDomain(0 to 1)),
      new Variable("v1", IntDomain(1 to 5)),
      v2), false, pm)

    val pb = Problem(c.scope: _*)
    pb.addConstraint(c)
    val ps = pb.initState.toState

    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)
    mod.dom(v2) should contain theSameElementsAs (0 to 1)

  }

  it should "detect singleton inconsistency" in {
    val v0 = new Variable("v0", Singleton(1))
    val pb = Problem(v0)

    forAll(Seq(
      LinearLe(0, Array(1), Array(v0), false, pm),
      new LinearNe(1, Array(1), Array(v0)),
      new LinearEq(0, Array(1), Array(v0)))) { c =>
      pb.addConstraint(c)
      val ps = pb.initState.toState
      c.adviseAll(ps)
      c.revise(ps) shouldBe Contradiction
    }

  }

  it should "filter /=" in {
    val v0 = new Variable("v0", IntDomain(-10 to 10))
    val pb = Problem(v0)
    val c = new LinearNe(2, Array(2), Array(v0))
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)
    mod.dom(v0) should contain theSameElementsAs (-10 to 10 filter (_ != 1))

  }

  it should "not filter /=" in {
    val v0 = new Variable("v0", IntDomain(-10 to 10))
    val v1 = new Variable("v1", IntDomain(-1 to 1))
    val pb = Problem(v0, v1)
    val c = new LinearNe(0, Array(1, 1), Array(v0, v1))
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)
    mod shouldBe ps

  }

  it should "filter = with particular parameters" in {
    val q43 = new Variable("q43", IntDomain(0 to 3))
    val q44 = new Variable("q44", IntDomain(1 to 1))
    val q45 = new Variable("q45", IntDomain(1 to 2))
    val q46 = new Variable("q46", IntDomain(0 to 0))
    val q47 = new Variable("q47", IntDomain(0 to 0))
    val q48 = new Variable("q48", IntDomain(0 to 0))

    val c = new LinearEq(-6, Array(-1, -2, -3, -4, -5, -6), Array(q43, q44, q45, q46, q47, q48))
    val pb = Problem(c.scope: _*)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)
    mod.dom(q43) should contain theSameElementsAs Seq(1)

    // -1.Q[43] [0, 3] + -2.Q[44] [1] + -3.Q[45] [1, 2] + -4.Q[46] [0] + -5.Q[47] [0] + -6.Q[48] [0] eq -6 ()
  }

  it should "filter = with other particular parameters" in {
    val x98 = new Variable("x98", IntDomain.ofSeq(0, 2))
    val x1605 = new Variable("x1605", IntDomain(0 to 0))
    val x985 = new Variable("x985", IntDomain(-1 to 1))

    val c = new LinearEq(0, Array(1, -1, -1), Array(x98, x1605, x985))
    val pb = Problem(c.scope: _*)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)
    mod.dom(x985) should contain theSameElementsAs Seq(0)

    // -1.Q[43] [0, 3] + -2.Q[44] [1] + -3.Q[45] [1, 2] + -4.Q[46] [0] + -5.Q[47] [0] + -6.Q[48] [0] eq -6 ()
  }

  it should "filter = with yet other particular parameters" in {
    val x52 = new Variable("x52", IntDomain.ofSeq(0, 2, 4, 5))
    val x5 = new Variable("x5", Singleton(7))
    val x6 = new Variable("x6", IntDomain.ofSeq(3, 6, 8))

    val c = new LinearEq(0, Array(-1, 1, -1), Array(x52, x5, x6))
    val pb = Problem(c.scope: _*)
    pb.addConstraint(c)
    val ps = pb.initState.toState
    c.adviseAll(ps)
    val mod = c.consistentRevise(ps)
    withClue(c.toString(mod)) {
      mod.dom(x52) should contain theSameElementsAs Seq(4)
    }
    // -1.Q[43] [0, 3] + -2.Q[44] [1] + -3.Q[45] [1, 2] + -4.Q[46] [0] + -5.Q[47] [0] + -6.Q[48] [0] eq -6 ()
  }

  "Reified sum" should "detect inconsistency" in {
    val cspom = CSPOM { implicit cspom =>
      val r = new BoolVariable() as "r"
      val v0 = IntVariable(0 to 3) as "v0"
      ctr(CSPOMConstraint(r)('sum)(Seq(1), Seq(v0), -1) withParam ("mode" -> "le"))
    }

    val s = Solver(cspom).get

    val problem = s.concreteProblem

    val r = problem.variable("r")
    val v0 = problem.variable("v0")

    val state = problem.initState
      .assign(r, 1)
      .assign(v0, 0)
      .toState

    val Array(bc) = problem.constraints

    bc.adviseAll(state)
    bc.revise(state) shouldBe Contradiction
  }

}
