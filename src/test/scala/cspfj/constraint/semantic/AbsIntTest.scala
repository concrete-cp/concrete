package cspfj.constraint.semantic
import cspfj.Problem
import org.junit.Test
import cspfj.IntDomain
import org.junit.Assert._
import cspfj.AdviseCount

final class AbsIntTest {

  @Test
  def testX() {
    val prob = new Problem
    val x = prob.addVariable("x", IntDomain(-100 to 100))
    val y = prob.addVariable("y", IntDomain(-5))
    val c = new Abs(x, y)
    prob.addConstraint(c)
    AdviseCount.adviseAll(c)
    assertTrue(c.isBound)
    assertEquals(Seq(1), c.revise())
    assertEquals(Seq(5), x.dom.values.toSeq)
    assertTrue(c.isBound)
  }

  @Test
  def testY() {
    val prob = new Problem
    val x = prob.addVariable("x", IntDomain(7))
    val y = prob.addVariable("y", IntDomain(-100 to 100))
    val c = new Abs(x, y)
    prob.addConstraint(c)
    AdviseCount.adviseAll(c)
    assertTrue(c.isBound)
    assertEquals(Seq(2), c.revise())
    assertEquals(Seq(-7, 7), y.dom.values.toSeq)
    assertFalse(c.isBound)
    assertTrue(x.dom.bound)
  }

}