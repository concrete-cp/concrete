package cspfj.constraint.extension;

import org.junit.Before;
import org.junit.Test;
import org.junit.Assert._;

import cspfj.problem.IntDomain;
import cspfj.problem.Variable;

final class MatrixManager2DTest {

  private var matrix: MatrixManager2D = null;

  private var tuple: Array[Int] = null;

  @Before
  def setUp() {
    val var1 = new Variable("V1", new IntDomain(1, 2, 3));
    val var2 = new Variable("V2",
      new IntDomain(1, 2, 3, 4));
    tuple = new Array(2);

    val matrix2d = new Matrix2D(3, 4, false);
    matrix2d.set(Array(0, 0), true);
    matrix2d.set(Array(0, 2), true);

    matrix = new MatrixManager2D(Array(var1, var2), matrix2d,
      false, tuple);
  }

  @Test
  def testHasSupport() {
    assertTrue("(0,0)", matrix.hasSupport(0, 0));
    assertFalse("(0,1)", matrix.hasSupport(0, 1));
    assertFalse("(0,2)", matrix.hasSupport(0, 2));
    assertTrue("(1,0)", matrix.hasSupport(1, 0));
    assertFalse("(1,1)", matrix.hasSupport(1, 1));
    assertTrue("(1,2)", matrix.hasSupport(1, 2));
    assertFalse("(1,3)", matrix.hasSupport(1, 3));
  }

  @Test
  def testCheck() {
    Seq(0, 1).copyToArray(tuple);
    assertFalse(tuple.toList + " should be false", matrix.check);
    tuple(1) = 2;
    assertTrue(tuple.toString + " should be true", matrix.check);
  }

}
