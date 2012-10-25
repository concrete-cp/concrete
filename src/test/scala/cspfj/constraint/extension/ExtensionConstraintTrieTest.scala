package cspfj.constraint.extension;

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

import cspfj.IntDomain
import cspfj.Variable
import cspom.extension.HashTrie

final class ExtensionConstraintArrayTrieTest {

  @Test
  def testRestore() {
    val ta = new TupleTrieSet(MDD(), false);
    ta.set(Array(0, 0), true);
    ta.set(Array(1, 1), true);
    ta.set(Array(2, 2), true);

    val scope = Array(
      new Variable("V0", IntDomain(0, 1)),
      new Variable("V1", IntDomain(0, 1, 2)))

    val mmd = new ExtensionConstraintReduceable(scope, ArrayTrie(ta.toSeq: _*));
    val content = mmd.trie
    //println(content map (_.toSeq) mkString (", "))

    mmd.setLevel(1);

    mmd.revise()

    assertEquals(
      HashTrie(Array(0, 0), Array(1, 1)),
      mmd.trie);
    assertEquals(2, mmd.trie.size);

    mmd.restoreLevel(0)

    assertEquals(mmd.trie, content)
  }
}
