package cspfj.constraint.extension;

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import java.util.Arrays
import java.util.HashSet
import java.util.Iterator
import java.util.Set
import org.junit.Before
import org.junit.Test
import cspfj.problem.BitVectorDomain
import cspfj.problem.Variable;
import scala.collection.JavaConversions

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
      new Variable("V0", new BitVectorDomain(0, 1, 2)),
      new Variable("V1", new BitVectorDomain(0, 1, 2)));

    mmd = new MatrixManagerDynamic(scope, ta, false, new Array[Int](2));
    content = mmd.toArray;
  }

  @Test
  def testRestore() {
    val itr2 = mmd.iterator
    mmd.level = 1;
    itr2.next();
    itr2.next();
    itr2.remove();
    itr2.next();
    itr2.remove();

    val itr = mmd.iterator
    assertArrayEquals(content(0), itr.next());
    assertFalse(itr.hasNext);

    mmd.level = 0

    val set = collection.mutable.Set.empty ++ content;
    val itr1 = mmd.iterator
    assertTrue(set.remove(itr1.next()));
    assertTrue(set.remove(itr1.next()));
    assertTrue(set.remove(itr1.next()));
    assertFalse(itr1.hasNext);
    assertTrue(set.isEmpty);

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

  @Test
  def testIterator2() {
    var itr2 = mmd.iterator;
    assertArrayEquals(content(0), itr2.next());
    itr2.remove();
    assertTrue(itr2.hasNext);
    assertArrayEquals(content(1), itr2.next());
    assertArrayEquals(content(2), itr2.next());
    assertFalse(itr2.hasNext);

    itr2 = mmd.iterator;
    assertArrayEquals(content(1), itr2.next());
    assertArrayEquals(content(2), itr2.next());
    itr2.remove();
    assertFalse(itr2.hasNext);

    itr2 = mmd.iterator;
    assertArrayEquals(content(1), itr2.next());
    itr2.remove();
    assertFalse(itr2.hasNext);

    itr2 = mmd.iterator
    assertFalse(itr2.hasNext);
  }

}
