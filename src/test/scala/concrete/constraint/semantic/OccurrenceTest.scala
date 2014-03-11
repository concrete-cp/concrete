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

class OccurrenceTest {
  @Test
  def test() {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val occ = new Variable("occ", IntDomain(1, 2, 3))

    val c = new OccurrenceVar(occ, 7, Array(v1, v2, v3, v4, v5))

    assertEquals(Seq(0), c.revise())

    assertEquals(List(1, 2), occ.dom.values.toList)

  }

  @Test(expected = classOf[UNSATException])
  def test2() {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val occ = new Variable("occ", IntDomain(3, 4, 5))

    val c = new OccurrenceVar(occ, 7, Array(v1, v2, v3, v4, v5))
    c.revise()

  }

  @Test
  def test3() {
    val problem = CSPOM {
      val v1 = 7
      val v2 = 6
      val v3 = IntVariable.of(7, 9)
      val v4 = 4
      val v5 = IntVariable.of(8, 9)

      val occ = IntVariable.ofInterval(1, 3) as "occ"
      ctr(occ === occurrence(7, v1, v2, v3, v4, v5))
    }

    val s = Solver(problem)
    val c = s.problem.constraints.collectFirst {
      case c: OccurrenceVar => c
    } get
    val occ = s.problem.variable("occ")

    assertEquals(Seq(0), c.revise())

    assertEquals(List(1, 2), occ.dom.values.toList)

  }

}