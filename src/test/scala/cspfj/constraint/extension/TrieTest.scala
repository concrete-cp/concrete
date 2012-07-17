package cspfj.constraint.extension;

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

import cspfj.IntDomain
import cspfj.Variable
import cspom.extension.Trie

final class TrieTest {

  val t = Trie.empty(3) + Array(1, 2, 3) + Array(1, 3, 4) + Array(1, 2, 5) + Array(2, 3, 5)

  @Test
  def testTrie() {
    assertEquals(4, t.size)

    assertTrue(t.contains(Array(1, 3, 4)))
    assertFalse(t.contains(Array(1, 2, 4)))

    var s = Trie.empty(3) + Array(1, 2, 5) + Array(1, 3, 4) + Array(1, 2, 3)

    assertFalse(t == s)

    s += Array(2, 3, 5)

    assertEquals(t, s)

    var u = Trie(
      Array(1, 2, 3),
      Array(1, 3, 4),
      Array(1, 2, 5),
      Array(2, 3, 5))

    assertEquals(t, u)

  }

  @Test
  def testRestore() {
    val ta = new TupleTrieSet(Trie.empty(2), false);
    ta.set(Array(0, 0), true);
    ta.set(Array(1, 1), true);
    ta.set(Array(2, 2), true);

    val scope = Array(
      new Variable("V0", IntDomain(0, 1)),
      new Variable("V1", IntDomain(0, 1, 2)))

    val mmd = new ExtensionConstraintTrie(scope, ta);
    val content = mmd.trie
    //println(content map (_.toSeq) mkString (", "))

    mmd.setLevel(1);

    mmd.revise()

    assertEquals(
      Trie(Array(0, 0), Array(1, 1)),
      mmd.trie);
    assertEquals(2, mmd.trie.size);

    mmd.restoreLevel(0)

    assertEquals(mmd.trie, content)
  }
}
