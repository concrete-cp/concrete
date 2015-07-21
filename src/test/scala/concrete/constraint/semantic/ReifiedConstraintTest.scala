package concrete.constraint.semantic

import scala.annotation.varargs
import scala.reflect.runtime.universe

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.BooleanDomain
import concrete.Contradiction
import concrete.IntDomain
import concrete.Problem
import concrete.ProblemState
import concrete.Singleton
import concrete.Solver
import concrete.TRUE
import concrete.UNKNOWNBoolean
import concrete.Variable
import concrete.constraint.AdviseCount
import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable

class ReifiedConstraintTest extends FlatSpec with Matchers {

  "Reified neq" should "be revised after advise" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(0))
    val v1 = new Variable("v1", IntDomain.ofSeq(3, 4))
    val control1 = new Variable("control1", BooleanDomain())
    val control2 = new Variable("control2", BooleanDomain())
    val c1 = new ReifiedConstraint(
      control1,
      new Neq(v0, v1),
      new EqAC(v0, v1))

    val c2 = new ReifiedConstraint(
      control2,
      new Neq(v0, v1),
      new EqBC(v0, v1))

    val ac = new AdviseCount
    c1.register(ac)
    c2.register(ac)

    val pb = Problem(control1, control2, v0, v1)
    pb.addConstraints(Seq(c1, c2))
    val ps = pb.initState.toState

    //c1.advise(ps, 2) should be >= 0
    c2.advise(ps, 2) should be >= 0

    val m2 = c2.consistentRevise(ps)

    val m1 = c1.consistentRevise(ps)

    m2.dom(control2) shouldBe TRUE
    assert(m2.isEntailed(c2))

    m1.dom(control1) shouldBe TRUE
    assert(m1.isEntailed(c1))

  }

  "Reified bound consistency eq" should "detect consistency" in {
    val v0 = new Variable("v0", IntDomain.ofInterval(1, 3))
    val v1 = new Variable("v1", Singleton(0))
    val control = new Variable("control", BooleanDomain())
    val constraint = new ReifiedConstraint(
      control,
      new EqBC(false, v0, -1, v1),
      new SumNE(1, Array(1, -1), Array(v0, v1)))
    val pb = Problem(v0, v1, control)
    pb.addConstraint(constraint)
    val state = pb.initState.toState
    constraint.adviseAll(state)
    constraint.revise(state) match {
      case Contradiction    => fail()
      case ns: ProblemState => ns.dom(control) shouldBe UNKNOWNBoolean
    }
  }

  "Reified sum" should "detect consistency" in {
    val cspom = CSPOM { implicit cspom =>
      val r = new BoolVariable() as "r"
      val v0 = IntVariable(1 to 3) as "v0"
      val v1 = IntVariable(0 to 3) as "v1"
      ctr(CSPOMConstraint(r)('sum)(Seq(1, -1), Seq(v0, v1), 1) withParam ("mode" -> "eq"))
    }

    val s = Solver(cspom).get

    val problem = s.concreteProblem

    val r = problem.variable("r")
    val v0 = problem.variable("v0")
    val v1 = problem.variable("v1")

    val state = problem.initState
      .assign(v1, 0)
      .assign(v0, 1)
      .toState

    withClue(problem.toString(state)) {
      val Array(ac, bc) = problem.constraints

      //    ac.adviseAll(state)
      //    ac.revise(state) match {
      //      case Contradiction    => fail()
      //      case ns: ProblemState => ns.dom(r) shouldBe TRUE
      //    }

      bc.adviseAll(state)
      bc.revise(state) match {
        case Contradiction    => fail()
        case ns: ProblemState => ns.dom(r) shouldBe TRUE
      }
    }
  }

}