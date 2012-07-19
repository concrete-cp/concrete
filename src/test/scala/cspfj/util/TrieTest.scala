package cspfj.util;

import java.util.Iterator;

import org.junit.Before;
import org.junit.Test;

import scala.collection.JavaConversions;
import org.junit.Assert._

import cspom.extension.Trie;
import cspom.extension.Trie$;

class TrieTest {

  private var ts: Trie = null

  @Before
  def setUp() {
    ts = Trie.empty;
    ts += (0, 0)
    ts += (0, 1)
    ts += (1, 0)
  }

  @Test
  def testContainsTuple() {
    assertTrue(ts.contains(Array(0, 1)));
    assertFalse(ts.contains(Array(1, 1)));
  }

  @Test
  def testRemoveTuple() {
    ts -= Array(0, 1);
    assertFalse(ts.contains(Array(0, 1)));
    ts -= Array(1, 1);
  }

  @Test
  def testIterator() {
    var i = 0;
    val itr = ts.iterator
    while (itr.hasNext) {
      itr.next();
      i += 1;
    }
    assertEquals(i, ts.size)
  }

  @Test
  def testSize() {
    assertEquals(3, ts.size);
    ts -= (0, 0);
    assertEquals(2, ts.size);
    ts += (1, 1);
    assertEquals(3, ts.size);
  }
}
