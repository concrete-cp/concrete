package concrete
package constraint
package semantic

import concrete.constraint.linear.{EqACFast, EqBC, LinearNe, StatelessLinearEq}
import cspom.CSPOM._
import cspom.variable.{BoolVariable, IntVariable}
import cspom.{CSPOM, CSPOMConstraint}
import org.scalatest.{FlatSpec, Matchers}

class ReifiedConstraintTest extends FlatSpec with Matchers {

  "Reified neq" should "be revised after advise" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(0, 1))
    val v1 = new Variable("v1", IntDomain.ofSeq(3, 4))
    val control1 = new Variable("control1", BooleanDomain())
    val control2 = new Variable("control2", BooleanDomain())
    val c11 = new ReifiedConstraint(
      neg = false,
      control1,
      new Neq(v0, v1))

    val c21 = new ReifiedConstraint(
      neg = true,
      control1,
      new EqACFast(v0, v1))

    val c12 = new ReifiedConstraint(
      neg = false,
      control2,
      new Neq(v0, v1))

    val c22 = new ReifiedConstraint(
      neg = true,
      control2,
      new EqBC(v0, v1))

    val ac = new AdviseCount
    Seq(c11, c21, c12, c22).foreach(_.register(ac))


    val pb = Problem(control1, control2, v0, v1)
    pb.addConstraints(Seq(c11, c12, c21, c22))
    val ps = pb.initState.assign(v0, 0)

    val m2 = ps.andThen { ps =>
      //c1.advise(ps, 2) should be >= 0
      c12.event(ps, Assignment, 1) should be >= 0
      c12.revise(ps)
    }
      .andThen { ps =>
        c22.event(ps, Assignment, 1) should be >= 0
        c22.revise(ps)
      }

    m2.dom(control2) shouldBe BooleanDomain.TRUE
    assert(m2.toState.entailed.hasInactiveVar(c22))

    val m1 = ps.andThen { ps =>
      c11.eventAll(ps)
      c11.revise(ps)
    }
      .andThen { ps =>
        c21.eventAll(ps)
        c21.revise(ps)
      }


    m1.dom(control1) shouldBe BooleanDomain.TRUE
    assert(m1.toState.entailed.hasInactiveVar(c21))

  }

  "Reified bound consistency eq" should "detect consistency" in {
    val v0 = new Variable("v0", IntDomain.ofInterval(1, 3))
    val v1 = new Variable("v1", Singleton(0))
    val control = new Variable("control", BooleanDomain())

    val constraint1 = new ReifiedConstraint(
      neg = false,
      control,
      new EqBC(false, v0, -1, v1))

    val constraint2 = new ReifiedConstraint(
      neg = true,
      control,
      new LinearNe(1, Array(1, -1), Array(v0, v1)))

    val pb = Problem(v0, v1, control)
    pb.addConstraint(constraint1)
    pb.addConstraint(constraint2)
    constraint1.register(new AdviseCount)
    constraint2.register(new AdviseCount)

    val ns = pb.initState
      .andThen { state =>
        constraint1.eventAll(state)
        constraint1.revise(state)
      }
      .andThen { state =>
        constraint2.eventAll(state)
        constraint2.revise(state)
      }

    ns.dom(control) shouldBe BooleanDomain.UNKNOWNBoolean

  }

  "Reified sum" should "detect consistency" in {
    val cspom = CSPOM { implicit cspom =>
      val r = new BoolVariable() as "r"
      val v0 = IntVariable(1 to 3) as "v0"
      val v1 = IntVariable(0 to 3) as "v1"
      ctr(CSPOMConstraint(r)("sum")(Seq(1, -1), Seq(v0, v1), 1) withParam ("mode" -> "eq"))
    }


    val s = Solver(cspom).get

    val problem = s.concreteProblem

    val r = problem.variable("r")
    val v0 = problem.variable("v0")
    val v1 = problem.variable("v1")

    val ns = problem.initState
      .assign(v1, 0)
      .assign(v0, 1)
      .andThen { state =>
        withClue(problem.toString(state)) {
          state.fold {
            problem.constraints.collect {
              case c: ReifiedConstraint => c
            }
          } { (state, bc) =>
            bc.eventAll(state)
            bc.revise(state)
          }
        }
      }

    ns.dom(r) shouldBe BooleanDomain.TRUE


  }

  it should "filter" in {
    val cspom = CSPOM { implicit cspom =>
      val r = new BoolVariable() as "r"
      val v0 = IntVariable(0 to 3) as "v0"
      val v1 = IntVariable(0, 1) as "v1"
      val v2 = IntVariable(0, 1) as "v2"
      ctr(CSPOMConstraint(r)("sum")(Seq(1, 1, 1), Seq(v0, v1, v2), 0) withParam ("mode" -> "eq"))
    }

    val s = Solver(cspom).get

    val problem = s.concreteProblem

    val r = problem.variable("r")
    val v0 = problem.variable("v0")
    val v1 = problem.variable("v1")
    val v2 = problem.variable("v2")

    val state = problem.initState
      .assign(v1, 0)
      .assign(v2, 0)
      .toState

    withClue(problem.toString(state)) {

      val Seq(bc) = problem.constraints.toSeq.collect {
        case c: ReifiedConstraint if c.constraint.isInstanceOf[StatelessLinearEq] => c
      }

      bc.revise(state)
        .orElse(fail())
        .andThen { ns =>
          ns.domains shouldBe state.domains
          ns.assign(r, 1)
        }
        .andThen { ps =>
          bc.event(ps, Assignment, 0) should be >= 0
          bc.revise(ps)
        }
        .orElse(fail())
        .andThen { ns =>
          ns.dom(v0).view should contain theSameElementsAs Seq(0)
          ns
        }
    }
  }

  it should "not filter" in {
    val cspom = CSPOM { implicit cspom =>
      val r = new BoolVariable() as "r"
      val v0 = IntVariable(0 to 3) as "v0"
      ctr(CSPOMConstraint(r)("sum")(Seq(-1), Seq(v0), -1) withParam ("mode" -> "le"))
    }

    val s = Solver(cspom).get

    val problem = s.concreteProblem

    val state = problem.initState.toState

    state.fold(problem.constraints) { (state, bc) =>
      bc.eventAll(state)
      bc.revise(state) match {
        case _: Contradiction => fail()
        case ns: ProblemState =>
          ns.domains shouldBe state.domains
          ns
      }
    }
  }

}