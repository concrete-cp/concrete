package concrete.constraint.semantic;

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import concrete.constraint.Constraint
import concrete.IntDomain
import concrete.Variable
import concrete.AdviseCount

import concrete.constraint.semantic.FilterSum._

final class ZeroSumTest {
  val v0 = new Variable("v0", IntDomain(1 to 4))
  val v1 = new Variable("v1", IntDomain(0 to 4))
  val b = new Variable("b", IntDomain(1))

  @Test
  def testLe3() {
    val x = new Variable("x", IntDomain(0, 1))
    val y = new Variable("y", IntDomain(0, 1))
    val c = new Sum(-19, Array(-20, -18), Array(x, y), SumLE)
    AdviseCount.adviseAll(c)
    c.revise()
    assertEquals(Seq(1), x.dom.values.toSeq)
    assertEquals(0 to 1, y.dom.values.toSeq)
  }

  @Test
  def reviseEq() {
    val c = new Sum(0, Array(4, -1, -1), Array(b, v0, v1), SumEQ);
    AdviseCount.adviseAll(c)
    c.revise()

    assertEquals(Seq(1, 2, 3, 4), v0.dom.values.toSeq);
    assertEquals(Seq(0, 1, 2, 3), v1.dom.values.toSeq);
  }

  @Test
  def reviseLe() {
    val c = new Sum(0, Array(4, -1, -1), Array(b, v0, v1), SumLE);
    AdviseCount.adviseAll(c)
    c.revise()

    assertEquals(Seq(1, 2, 3, 4), v0.dom.values.toSeq);
    assertEquals(Seq(0, 1, 2, 3, 4), v1.dom.values.toSeq);
  }

  @Test
  def testLe() {
    val c = new Sum(3, Array(1, 1), Array(v0, v1), SumLE)
    AdviseCount.adviseAll(c)
    c.revise()
    assertEquals(1 to 3, v0.dom.values.toSeq)
    assertEquals(0 to 2, v1.dom.values.toSeq)
  }

  @Test
  def testLe4() {
    val v2 = new Variable("v2", IntDomain(0 to 1))
    val c = new Sum(-3, Array(-1, -1, -6), Array(
      new Variable("v0", IntDomain(0 to 1)),
      new Variable("v1", IntDomain(1 to 5)),
      v2), SumLE)
    AdviseCount.adviseAll(c)
    c.revise()
    assertEquals(0 to 1, v2)
  }

}
