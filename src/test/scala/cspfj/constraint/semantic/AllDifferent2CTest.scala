package cspfj.constraint.semantic
import org.junit.Test
import cspfj.problem.IntDomain
import cspfj.problem.Variable
import cspfj.UNSATException

class AllDifferent2CTest {
  @Test(expected = classOf[UNSATException])
  def test() {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val c = new AllDifferent2C(v1, v2, v3, v4, v5)

    c.revise()
  }
}