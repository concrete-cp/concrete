package concrete.constraint.extension

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

final class MDDTest {

  private var ts: MDD = null

  @Before
  def setUp() {
    ts = MDD(Seq(Array(0, 0), Array(0, 1), Array(1, 0)))
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
    assertEquals(ts.lambda, i)
  }

  @Test
  def testSize() {
    assertEquals(BigInt(3), ts.lambda);
    //    ts -= (0, 0);
    //    assertEquals(2, ts.size);
    //    ts += (1, 1);
    //    assertEquals(3, ts.size);
  }

  @Test
  def testTrie() {
    val t = MDD0 + Array(1, 2, 3) + Array(1, 3, 4) + Array(1, 2, 5) + Array(2, 3, 5)
    assertEquals(BigInt(4), t.lambda)

    assertTrue(t.contains(Array(1, 3, 4)))
    assertFalse(t.contains(Array(1, 2, 4)))

    var s = MDD0 + Array(1, 2, 5) + Array(1, 3, 4) + Array(1, 2, 3)
    s += Array(2, 3, 5)

    assertTrue(t.lambda == s.lambda)
    assertTrue(t.iterator.map(_.toArray).forall(s.contains))

    var u = MDD(Seq(
      Array(1, 2, 3),
      Array(1, 3, 4),
      Array(1, 2, 5),
      Array(2, 3, 5)))

    assertTrue(t.lambda == u.lambda)
    assertTrue(t.iterator.map(_.toArray).forall(u.contains))

  }
}
