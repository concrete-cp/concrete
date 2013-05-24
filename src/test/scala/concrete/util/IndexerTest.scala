package concrete.util

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class IndexerTest {
  @Test
  def testDirect() {
    val i = Indexer.factory(0 to 10 toArray)
    assertTrue(i.isInstanceOf[DirectIndices])
    assertEquals(3, i.index(3))
    assertEquals(3, i.value(3))
    assertEquals(-1, i.index(-1))
    assertEquals(-1, i.index(11))
  }

  @Test
  def testOffset() {
    val i = Indexer.factory(1 to 10 toArray)
    assertTrue(i.isInstanceOf[OffsetIndices])
    assertEquals(3, i.index(4))
    assertEquals(4, i.value(3))
    assertEquals(-1, i.index(0))
    assertEquals(-1, i.index(11))
  }
}
