package concrete.constraint.semantic
import concrete.Problem
import org.junit.Test
import concrete.IntDomain
import org.junit.Assert._
import concrete.AdviseCount
import concrete.Variable

final class AddIntTest {

  @Test
  def testX() {

    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain(5))
    val z = new Variable("z", IntDomain(2))
    val prob = new Problem(x, y, z)
    val c = new AddAC(x, y, z)
    prob.addConstraint(c)
    c.adviseAll()
    assertTrue(c.intervalsOnly)
    assertEquals(Seq(0), c.revise())
    assertEquals(Seq(7), x.dom.values.toSeq)
    assertTrue(c.intervalsOnly)
  }

  @Test
  def testY() {

    val x = new Variable("x", IntDomain(7))
    val y = new Variable("y", IntDomain(-100 to 100))
    val z = new Variable("z", IntDomain(2))
    val prob = new Problem(x, y, z)
    val c = new AddAC(x, y, z)
    prob.addConstraint(c)
    c.adviseAll()
    assertTrue(c.intervalsOnly)
    assertEquals(Seq(1), c.revise())
    assertEquals(Seq(5), y.dom.values.toSeq)
    assertTrue(c.intervalsOnly)
  }

  @Test
  def testZ() {

    val x = new Variable("x", IntDomain(1 to 10))
    val y = new Variable("y", IntDomain(20 to 30))
    val z = new Variable("z", IntDomain(-100 to 100))
    val prob = new Problem(x, y, z)
    val c = new AddAC(x, y, z)
    prob.addConstraint(c)
    c.adviseAll()
    assertTrue(c.intervalsOnly)
    assertEquals(Seq(2), c.revise())
    assertEquals((-29 to -10).toSeq, z.dom.values.toSeq)
    assertTrue(c.intervalsOnly)
  }
}