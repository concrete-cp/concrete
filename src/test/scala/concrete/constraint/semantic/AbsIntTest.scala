package concrete.constraint.semantic
import concrete.Problem
import org.junit.Test
import concrete.IntDomain
import org.junit.Assert._
import concrete.AdviseCount
import concrete.Variable

final class AbsIntTest {

  @Test
  def testX() {

    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain(-5))
    val prob = new Problem(x, y)
    val c = new Abs(x, y)
    prob.addConstraint(c)
    c.adviseAll()
    assertTrue(c.intervalsOnly)
    assertEquals(Seq(0), c.revise())
    assertEquals(Seq(5), x.dom.values.toSeq)
    assertTrue(c.intervalsOnly)
  }

  @Test
  def testY() {

    val x = new Variable("x", IntDomain(7))
    val y = new Variable("y", IntDomain(-100 to 100))
    val prob = new Problem(x, y)
    val c = new Abs(x, y)
    prob.addConstraint(c)
    c.adviseAll()
    assertTrue(c.intervalsOnly)
    assertEquals(Seq(1), c.revise())
    assertEquals(Seq(-7, 7), y.dom.values.toSeq)
    assertFalse(c.intervalsOnly)
    assertTrue(x.dom.bound)
  }

}