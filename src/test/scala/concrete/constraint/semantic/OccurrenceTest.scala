package concrete.constraint.semantic
import org.junit.Test
import concrete.IntDomain
import concrete.Variable
import concrete.UNSATException
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import cspom.CSPOM
import CSPOM._
import concrete.CSPOMDriver._
import cspom.variable.CSPOMExpression
import concrete.Solver
import cspom.compiler.ProblemCompiler
import concrete.generator.cspompatterns.ConcretePatterns
import cspom.variable.IntVariable
import concrete.Revised
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import concrete.Contradiction

class OccurrenceTest extends FlatSpec with Matchers with Inspectors {

  "Occurrence" should "filter" in {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val occ = new Variable("occ", IntDomain(1, 2, 3))

    val c = new OccurrenceVar(occ, 7, Array(v1, v2, v3, v4, v5))

    def domains = Array(occ, v1, v2, v3, v4, v5).map(_.initDomain)

    val Revised(mod, _, _) = c.revise(domains, Unit)

    forAll((mod zip domains).drop(1)) {
      case (m, d) => m should be theSameInstanceAs d
    }

    mod(0) shouldBe Seq(1, 2)

  }

  it should "detect contradiction" in {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val occ = new Variable("occ", IntDomain(3, 4, 5))

    val c = new OccurrenceVar(occ, 7, Array(v1, v2, v3, v4, v5))
    def domains = Array(occ, v1, v2, v3, v4, v5).map(_.initDomain)

    c.revise(domains, Unit) shouldBe Contradiction

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

    val domains = c.scope.map(_.initDomain)

    val Revised(mod, _, _) = c.revise(domains, Unit)

    forAll((mod zip domains).drop(1)) {
      case (m, d) => m should be theSameInstanceAs d
    }

    mod(0) shouldBe Seq(1, 2)

  }

}
