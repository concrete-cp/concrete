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
  private var content: Array[Array[Int]] = null;

  @Before
  def setUp() {
    val ta = new TupleSet(false);
    ta.set(Array(0, 0), true);
    ta.set(Array(1, 1), true);
    ta.set(Array(2, 2), true);

    val scope = Array(
      new Variable("V0", new IntDomain(0, 1, 2)),
      new Variable("V1", new IntDomain(0, 1, 2)));

    mmd = new MatrixManagerDynamic(scope, ta, false, new Array[Int](2));
    content = mmd.toArray;
    //println(content map (_.toSeq) mkString (", "))
  }

  @Test
  def testRestore() {
    mmd.setLevel(1);
    mmd.filterTuples(t => t sameElements List(0, 0))

    val itr = mmd.iterator
    assertArrayEquals(Array(0, 0), itr.next());
    assertFalse(itr.hasNext);

    mmd.restoreLevel(0)

    assertEquals(mmd.iterator.toSet, content.toSet)

  }

  @Test
  def testIterator1() {
    val itr1 = mmd.iterator;
    assertTrue(itr1.hasNext);
    assertArrayEquals(content(0), itr1.next());
    assertTrue(itr1.hasNext);
    assertArrayEquals(content(1), itr1.next());
    assertTrue(itr1.hasNext);
    assertArrayEquals(content(2), itr1.next());
    assertFalse(itr1.hasNext);
  }

  //  @Test
  //  def testIterator2() {
  //    var itr2 = mmd.iterator;
  //    assertArrayEquals(content(0), itr2.next());
  //    itr2.remove();
  //    assertTrue(itr2.hasNext);
  //    assertArrayEquals(content(1), itr2.next());
  //    assertArrayEquals(content(2), itr2.next());
  //    assertFalse(itr2.hasNext);
  //
  //    itr2 = mmd.iterator;
  //    assertArrayEquals(content(1), itr2.next());
  //    assertArrayEquals(content(2), itr2.next());
  //    itr2.remove();
  //    assertFalse(itr2.hasNext);
  //
  //    itr2 = mmd.iterator;
  //    assertArrayEquals(content(1), itr2.next());
  //    itr2.remove();
  //    assertFalse(itr2.hasNext);
  //
  //    itr2 = mmd.iterator
  //    assertFalse(itr2.hasNext);
  //  }

}
