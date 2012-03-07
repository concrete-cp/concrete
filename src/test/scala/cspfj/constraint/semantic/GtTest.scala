package cspfj.constraint.semantic;

import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test

import cspfj.problem.IntDomain
import cspfj.problem.Variable

final class GtTest {
  var v1: Variable = null
  var v2: Variable = null

  @Before
  def setUp() {
    v1 = new Variable("v1", new IntDomain(1, 2, 3, 4));
    v2 = new Variable("v2", new IntDomain(3, 4, 5));
  }

  @Test
  def testRevise1() {

    val c = new Gt(v1, v2, true);
    c.revise();

    assertEquals(Seq(4), v1.dom.values.toSeq);
    assertEquals(Seq(3), v2.dom.values.toSeq);

  }

  @Test
  def testRevise2() {
    val c = new Gt(v1, v2, false);
    c.revise();

    assertEquals(Seq(3, 4), v1.dom.values.toSeq);
    assertEquals(Seq(3, 4), v2.dom.values.toSeq);

  }

}
