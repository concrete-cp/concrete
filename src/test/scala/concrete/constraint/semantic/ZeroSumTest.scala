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
  def reviseGe() {
    val c = new Sum(0, Array(4, -1, -1), Array(b, v0, v1), SumGE);
    AdviseCount.adviseAll(c)
    c.revise()

    assertEquals(Seq(1, 2, 3, 4), v0.dom.values.toSeq);
    assertEquals(Seq(0, 1, 2, 3), v1.dom.values.toSeq);
  }
}
