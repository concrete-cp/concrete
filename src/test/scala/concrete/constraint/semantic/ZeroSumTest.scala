package concrete.constraint.semantic;

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import concrete.constraint.Constraint
import concrete.IntDomain
import concrete.Variable
import concrete.constraint.AdviseCount

import concrete.constraint.semantic.SumMode._

final class ZeroSumTest {
  val v0 = new Variable("v0", IntDomain(1 to 4))
  val v1 = new Variable("v1", IntDomain(0 to 4))
  val b = new Variable("b", IntDomain(1))

  @Test
  def testLe3() {
    val x = new Variable("x", IntDomain(0, 1))
    val y = new Variable("y", IntDomain(0, 1))
    val c = new Sum(-19, Array(-20, -18), Array(x, y), SumLE)
    c.adviseAll()
    c.revise()
    assertEquals(Seq(1), x.dom.values.toSeq)
    assertEquals(0 to 1, y.dom.values.toSeq)
  }

  @Test
  def reviseEq() {
    val c = new Sum(0, Array(4, -1, -1), Array(b, v0, v1), SumEQ);
    c.adviseAll()
    c.revise()

    assertEquals(Seq(1, 2, 3, 4), v0.dom.values.toSeq);
    assertEquals(Seq(0, 1, 2, 3), v1.dom.values.toSeq);
  }

  @Test
  def reviseLe() {
    val c = new Sum(0, Array(4, -1, -1), Array(b, v0, v1), SumLE);
    c.adviseAll()
    c.revise()

    assertEquals(Seq(1, 2, 3, 4), v0.dom.values.toSeq);
    assertEquals(Seq(0, 1, 2, 3, 4), v1.dom.values.toSeq);
  }

  @Test
  def testLe() {
    val c = new Sum(3, Array(1, 1), Array(v0, v1), SumLE)
    c.adviseAll()
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
    c.adviseAll()
    c.revise()
    assertEquals(0 to 1, v2.dom.values.toSeq)
  }
  
  @Test 
  def testEq() {
    val q43 = new Variable("q43", IntDomain(0 to 3))
    val q44 = new Variable("q44", IntDomain(1 to 1))
    val q45 = new Variable("q45", IntDomain(1 to 2))
    val q46 = new Variable("q46", IntDomain(0 to 0))
    val q47 = new Variable("q47", IntDomain(0 to 0))
    val q48 = new Variable("q48", IntDomain(0 to 0))
    
    val c = new Sum(-6, Array(-1,-2,-3,-4,-5,-6), Array(q43, q44, q45, q46, q47, q48), SumEQ)
    c.adviseAll()
    c.revise()
    
    assertEquals(c.toString, Seq(1), q43.dom.values.toSeq)
    
    
    // -1.Q[43] [0, 3] + -2.Q[44] [1] + -3.Q[45] [1, 2] + -4.Q[46] [0] + -5.Q[47] [0] + -6.Q[48] [0] eq -6 ()
  }

}
