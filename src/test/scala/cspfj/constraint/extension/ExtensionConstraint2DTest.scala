package cspfj.constraint.extension;

import org.junit.Before;
import org.junit.Test;
import org.junit.Assert._;

import cspfj.IntDomain;
import cspfj.Variable;

final class ExtensionConstraint2DTest {

  private var c: BinaryExt = null;

  @Before
  def setUp() {
    val var1 = new Variable("V1", IntDomain(1, 2, 3))
    val var2 = new Variable("V2", IntDomain(1, 2, 3, 4))

    val matrix2d = new Matrix2D(3, 4, false);
    matrix2d.set(Array(0, 0), true);
    matrix2d.set(Array(0, 2), true);

    c = new BinaryExt(Array(var1, var2), matrix2d, false)

  }

  @Test
  def testHasSupport() {
    assertTrue("(0,0)", c.hasSupport(0, 0));
    assertFalse("(0,1)", c.hasSupport(0, 1));
    assertFalse("(0,2)", c.hasSupport(0, 2));
    assertTrue("(1,0)", c.hasSupport(1, 0));
    assertFalse("(1,1)", c.hasSupport(1, 1));
    assertTrue("(1,2)", c.hasSupport(1, 2));
    assertFalse("(1,3)", c.hasSupport(1, 3));
  }

  @Test
  def testCheck() {
    assertFalse("(0,1) should be false", c.checkIndices(Array(0, 1)));
    assertTrue("(0,0) should be true", c.checkIndices(Array(0, 0)));
  }

}
