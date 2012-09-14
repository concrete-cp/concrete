package cspfj.constraint.extension

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

final class MDDTest {

  private var ts: MDD = null

  @Before
  def setUp() {
    ts = MDD();
    ts += Array(0, 0)
    ts += Array(0, 1)
    ts += Array(1, 0)
  }

  @Test
  def testContainsTuple() {
    assertTrue(ts.contains(Array(0, 1)));
    assertFalse(ts.contains(Array(1, 1)));
  }

//  @Test
//  def testRemoveTuple() {
//    ts -= Array(0, 1);
//    assertFalse(ts.contains(Array(0, 1)));
//    ts -= Array(1, 1);
//    assertEquals(2, ts.size)
//  }

  @Test
  def testIterator() {
    var i = 0;
    val itr = ts.iterator
    while (itr.hasNext) {
      itr.next();
      i += 1;
    }
    assertEquals(ts.size, i)
  }

  @Test
  def testSize() {
    assertEquals(3, ts.size);
    //    ts -= (0, 0);
    //    assertEquals(2, ts.size);
    //    ts += (1, 1);
    //    assertEquals(3, ts.size);
  }

  @Test
  def testTrie() {
    val t = ArrayTrie.empty + Array(1, 2, 3) + Array(1, 3, 4) + Array(1, 2, 5) + Array(2, 3, 5)
    assertEquals(4, t.size)

    assertTrue(t.contains(Array(1, 3, 4)))
    assertFalse(t.contains(Array(1, 2, 4)))

    var s = ArrayTrie.empty + Array(1, 2, 5) + Array(1, 3, 4) + Array(1, 2, 3)

    assertFalse(t == s)

    s += Array(2, 3, 5)

    assertEquals(t, s)

    var u = ArrayTrie(
      Array(1, 2, 3),
      Array(1, 3, 4),
      Array(1, 2, 5),
      Array(2, 3, 5))

    assertEquals(t, u)

  }
}
