package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import concrete.CSPOMDriver
import concrete.Contradiction
import concrete.IntDomain
import concrete.Problem
import concrete.Solver
import concrete.Variable
import cspom.CSPOM
import cspom.CSPOM.constant
import cspom.CSPOM.ctr
import cspom.variable.IntVariable
import concrete.constraint.AdviseCount
import org.scalatest.TryValues
import concrete.filter.ACC
import concrete.ParameterManager
import concrete.BoundRemoval

class AtMostTest extends FlatSpec with Matchers with Inspectors with TryValues {

  "AtMost" should "filter" in {
    val v1 = new Variable("V1", IntDomain.ofSeq(7))
    val v2 = new Variable("V2", IntDomain.ofSeq(6))
    val v3 = new Variable("V3", IntDomain.ofSeq(7, 9))
    val v4 = new Variable("V4", IntDomain.ofSeq(8))
    val v5 = new Variable("V5", IntDomain.ofSeq(8, 9))

    val occ = new Variable("occ", IntDomain.ofSeq(0, 1, 2, 3))

    val value = new Variable("value", IntDomain.ofSeq(7))

    val c = new AtMost(occ, value, Array(v1, v2, v3, v4, v5))
    c.register(new AdviseCount())
    val ps = Problem(occ, value, v1, v2, v3, v4, v5)
      .addConstraint(c)
      .initState

    ps.andThen { ps =>
      c.adviseAll(ps)
      c.revise(ps)
    }
      .andThen { mod =>
        forAll(c.scope.drop(1)) {
          case v => mod.dom(v) should be theSameInstanceAs v.initDomain
        }

        mod.dom(occ).view should contain theSameElementsAs Seq(1, 2, 3)
        mod
      }
      .orElse { fail("Inconsistency") }
      .andThen(_.removeFrom(occ, 2))
      .andThen { ps =>
        c.advise(ps, BoundRemoval, 0)
        c.revise(ps)
      }
      .andThen { ps =>
        ps.dom(v3).view should contain theSameElementsAs Seq(9)
        ps
      }
      .orElse { fail("Inconsistency") }

  }

  it should "detect contradiction" in {
    val v1 = new Variable("1", IntDomain.ofSeq(7))
    val v2 = new Variable("2", IntDomain.ofSeq(6))
    val v3 = new Variable("3", IntDomain.ofSeq(7, 9))
    val v4 = new Variable("4", IntDomain.ofSeq(8))
    val v5 = new Variable("5", IntDomain.ofSeq(8, 9))

    val occ = new Variable("occ", IntDomain.ofSeq(0))

    val value = new Variable("value", IntDomain.ofSeq(7))

    val c = new AtMost(occ, value, Array(v1, v2, v3, v4, v5))
    c.register(new AdviseCount())

    val pb = Problem(occ, v1, v2, v3, v4, v5)
    pb.addConstraint(c)

    pb.initState.andThen { ps =>
      c.adviseAll(ps)
      c.revise(ps)
    } shouldBe Contradiction

  }

  it should "generate and filter" in {
    val cspom = CSPOM { implicit problem =>
      val v1 = 7
      val v2 = 6

      val v3 = IntVariable(7, 9)
      val v4 = 4
      val v5 = IntVariable(8, 9)

      val occ = IntVariable(0 to 3) as "occ"

      ctr(CSPOMDriver.atMost(occ, 7, v1, v2, v3, v4, v5))
    }

    val problem = Solver(cspom).get.concreteProblem

    val c = problem.constraints
      .collectFirst {
        case c: AtMost => c
      }
      .get

    val occ = problem.variable("occ")
    val initState = problem.initState

    val mod = new ACC(problem, new ParameterManager()).reduceAll(initState.toState)

    forAll(problem.variables) {
      case `occ` => mod.dom(occ).view should contain theSameElementsAs Seq(1, 2, 3)
      case v => mod.dom(v) should be theSameInstanceAs initState.dom(v)
    }

  }

}
