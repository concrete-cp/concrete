package cspfj.constraint.semantic
import org.junit.Assert.assertEquals
import org.junit.Test

import cspfj.constraint.NULL_REVISATOR
import cspfj.problem.BitVectorDomain
import cspfj.problem.Variable

class EqTest {

  @Test
  def test() {
    val v0 = new Variable("v0", new BitVectorDomain(1, 2, 3))
    val v1 = new Variable("v1", new BitVectorDomain(3, 4, 5))
    val c = new Eq(v0, v1)
    c.revise(NULL_REVISATOR, 0)
    assertEquals(Seq(3), v0.dom.values.toSeq)
    assertEquals(Seq(3), v1.dom.values.toSeq)
  }
}