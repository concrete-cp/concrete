package cspfj.constraint.semantic
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import cspfj.IntDomain
import cspfj.Problem
import cspfj.Variable

class SatTest {
  @Test
  def test() {
    val problem = new Problem
    val v0 = new Variable("v0", IntDomain(0 to 2));
    val v1 = new Variable("v1", IntDomain(0 to 2));
    val v2 = new Variable("v2", IntDomain(0 to 2));

    val c = new Sat(v0, v1, v2)
    problem.addConstraint(c);

    val tuple = Array(0, 0, 0)

    assertTrue(c.checkIndices(Array(0, 0, 0)))
    c.noGood(Seq((v0, 0), (v1, 1)))
    assertFalse(c.checkIndices(Array(0, 1, 0)))

    v1.dom.remove(0)
    v1.dom.remove(2)
    assertFalse(c.findSupport(0, 0).isDefined)

    assertTrue(c.findSupport(0, 1).isDefined)
  }
}