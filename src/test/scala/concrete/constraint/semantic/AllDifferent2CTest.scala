package concrete.constraint.semantic
import org.junit.Test
import org.junit.Assert._
import concrete.IntDomain
import concrete.Variable
import concrete.UNSATException
import concrete.AdviseCount

class AllDifferent2CTest {
  @Test(expected = classOf[Exception])
  def test() {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val c = new AllDifferent2C(v1, v2, v3, v4, v5)

    AdviseCount.adviseAll(c)

    c.revise()
  }

  def testCheck() {
    val v1 = new Variable("1", IntDomain(6, 7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9, 10))

    val c = new AllDifferent2C(v1, v2, v3, v4, v5)
    
    assertTrue(c.checkValues(Array(7, 6, 9, 8, 10)))
    assertFalse(c.checkValues(Array(7, 6, 7, 8, 10)))

  }
}