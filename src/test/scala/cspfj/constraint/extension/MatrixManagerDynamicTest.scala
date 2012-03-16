package cspfj.constraint.extension;

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

import cspfj.problem.IntDomain
import cspfj.problem.Variable

final class MatrixManagerDynamicTest {

  private var mmd: MatrixManagerDynamic = null;
  private var content: List[Array[Int]] = null;

  @Before
  def setUp() {
    val ta = new TupleSet(false);
    ta.set(Array(0, 0), true);
    ta.set(Array(1, 1), true);
    ta.set(Array(2, 2), true);

    val scope = Array(
      new Variable("V0", IntDomain(0, 1, 2)),
      new Variable("V1", IntDomain(0, 1, 2)))

    mmd = new MatrixManagerDynamic(scope, ta, false, new Array[Int](2));
    content = mmd.tuples
    //println(content map (_.toSeq) mkString (", "))
  }

  @Test
  def testRestore() {
    mmd.setLevel(1);
    mmd.filterTuples(t => t sameElements List(0, 0))

    assertArrayEquals(Array(0, 0), mmd.tuples.head);
    assertEquals(1, mmd.tuples.size);

    mmd.restoreLevel(0)

    assertEquals(mmd.tuples.toSet, content.toSet)

  }

}
