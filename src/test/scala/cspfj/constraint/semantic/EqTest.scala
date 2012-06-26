package cspfj.constraint.semantic
import org.junit.Assert.assertEquals
import org.junit.Test

import cspfj.IntDomain
import cspfj.Variable

final class EqTest {

  @Test
  def test() {
    val v0 = new Variable("v0", IntDomain(1, 2, 3))
    val v1 = new Variable("v1", IntDomain(3, 4, 5))
    val c = new Eq(v0, v1)
    c.adviseAll()
    c.revise()
    assertEquals(Seq(3), v0.dom.values.toSeq)
    assertEquals(Seq(3), v1.dom.values.toSeq)
  }
}