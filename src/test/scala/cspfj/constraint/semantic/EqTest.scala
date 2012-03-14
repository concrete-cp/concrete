package cspfj.constraint.semantic
import org.junit.Assert.assertEquals
import org.junit.Test

import cspfj.problem.IntDomain
import cspfj.problem.Variable

final class EqTest {

  @Test
  def test() {
    val v0 = new Variable("v0", new IntDomain(Seq(1, 2, 3)))
    val v1 = new Variable("v1", new IntDomain(Seq(3, 4, 5)))
    val c = new Eq(v0, v1)
    c.fillRemovals()
    c.revise()
    assertEquals(Seq(3), v0.dom.values.toSeq)
    assertEquals(Seq(3), v1.dom.values.toSeq)
  }
}