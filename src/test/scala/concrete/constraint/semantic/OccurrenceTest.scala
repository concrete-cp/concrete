package concrete.constraint.semantic

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers

import concrete.CSPOMDriver.occurrence
import concrete.Contradiction
import concrete.IntDomain
import concrete.Problem
import concrete.Solver
import concrete.Variable
import cspom.CSPOM
import cspom.CSPOM.constant
import cspom.CSPOM.ctr
import cspom.variable.IntVariable

class OccurrenceTest extends FlatSpec with Matchers with Inspectors {

  "Occurrence" should "filter" in {
    val v1 = new Variable("1", IntDomain.ofSeq(7))
    val v2 = new Variable("2", IntDomain.ofSeq(6))
    val v3 = new Variable("3", IntDomain.ofSeq(7, 9))
    val v4 = new Variable("4", IntDomain.ofSeq(8))
    val v5 = new Variable("5", IntDomain.ofSeq(8, 9))

    val occ = new Variable("occ", IntDomain.ofSeq(1, 2, 3))

    val c = new OccurrenceVar(occ, 7, Array(v1, v2, v3, v4, v5))

    val ps = Problem(occ, v1, v2, v3, v4, v5).initState

    val mod = c.consistentRevise(ps)

    forAll(c.scope.drop(1)) {
      case v => mod.dom(v) should be theSameInstanceAs ps.dom(v)
    }

    mod.dom(occ) shouldBe Seq(1, 2)

  }

  it should "detect contradiction" in {
    val v1 = new Variable("1", IntDomain.ofSeq(7))
    val v2 = new Variable("2", IntDomain.ofSeq(6))
    val v3 = new Variable("3", IntDomain.ofSeq(7, 9))
    val v4 = new Variable("4", IntDomain.ofSeq(8))
    val v5 = new Variable("5", IntDomain.ofSeq(8, 9))

    val occ = new Variable("occ", IntDomain.ofSeq(3, 4, 5))

    val c = new OccurrenceVar(occ, 7, Array(v1, v2, v3, v4, v5))
    val ps = Problem(occ, v1, v2, v3, v4, v5).initState

    c.revise(ps) shouldBe Contradiction

  }

  it should "generate and filter" in {
    val problem = CSPOM { implicit problem =>
      val v1 = 7
      val v2 = 6

      val v3 = IntVariable.ofSeq(7, 9)
      val v4 = 4
      val v5 = IntVariable.ofSeq(8, 9)

      val occ = IntVariable(1 to 3) as "occ"

      ctr(occ === occurrence(7, v1, v2, v3, v4, v5))
    }

    val s = Solver(problem)
    val c = s.concreteProblem.constraints
      .collectFirst {
        case c: OccurrenceVar => c
      }
      .get

    val occ = s.concreteProblem.variable("occ")

    val mod = c.revise(s.concreteProblem.initState)

    forAll(s.concreteProblem.variables) {
      case `occ` => mod.dom(occ) shouldBe Seq(1, 2)
      case v     => mod.dom(v) should be theSameInstanceAs s.concreteProblem.initState.dom(v)
    }

  }

}
