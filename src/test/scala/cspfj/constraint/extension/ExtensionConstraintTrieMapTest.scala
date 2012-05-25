package cspfj.constraint.extension;

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

import cspfj.IntDomain
import cspfj.Variable

final class ExtensionConstraintTrieMapTest {

  //  @Before
  //  def setUp() {
  //    val ta = new TupleSet(false);
  //    ta.set(Array(0, 0), true);
  //    ta.set(Array(1, 1), true);
  //    ta.set(Array(2, 2), true);
  //
  //    val scope = Array(
  //      new Variable("V0", IntDomain(0, 1, 2)),
  //      new Variable("V1", IntDomain(0, 1, 2)))
  //
  //    mmd = new ExtensionConstraintTrie(scope, ta, false);
  //    content = mmd.tuples
  //    //println(content map (_.toSeq) mkString (", "))
  //  }
  //
  //  @Test
  //  def testRestore() {
  //    mmd.setLevel(1);
  //    mmd.filterTuples(t => t sameElements List(0, 0))
  //
  //    assertArrayEquals(Array(0, 0), mmd.tuples.head);
  //    assertEquals(1, mmd.tuples.size);
  //
  //    mmd.restoreLevel(0)
  //
  //    assertEquals(mmd.tuples.toSet, content.toSet)
  //
  //  }

  @Test
  def testTrie() {
    var t = TrieMap.empty(3) + List(1, 2, 3)
    t += List(1, 3, 4)
    t += List(1, 2, 5)
    t += List(2, 3, 5)

    assertEquals(4, t.size)
    assertTrue(t.contains(List(1, 3, 4)))
    assertTrue(t.contains(Array(1, 3, 4)))
    assertFalse(t.contains(List(1, 2, 4)))
    assertFalse(t.contains(Array(1, 2, 4)))

    var s = TrieMap.empty(3) + List(1, 2, 3)
    s += List(1, 3, 4)
    s += List(1, 2, 5)

    assertFalse(t == s)

    s += List(2, 3, 5)

    assertEquals(t, s)

    var u = TrieMap.of(List(
      Array(1, 2, 3),
      Array(1, 3, 4),
      Array(1, 2, 5),
      Array(2, 3, 5)))

    assertEquals(t, u)

  }

  @Test
  def testRestore() {
    val ta = new TupleSet(false);
    ta.set(Array(0, 0), true);
    ta.set(Array(1, 1), true);
    ta.set(Array(2, 2), true);

    val scope = Array(
      new Variable("V0", IntDomain(0, 1)),
      new Variable("V1", IntDomain(0, 1, 2)))

    val mmd = new ExtensionConstraintTrie(scope, TrieMap.of(ta));
    val content = mmd.trie
    //println(content map (_.toSeq) mkString (", "))

    mmd.setLevel(1);

    mmd.revise()

    assertEquals(
      TrieMap.of(List(Array(0, 0), Array(1, 1))),
      mmd.trie);
    assertEquals(2, mmd.trie.size);

    mmd.restoreLevel(0)

    assertEquals(mmd.trie, content)
  }
}
