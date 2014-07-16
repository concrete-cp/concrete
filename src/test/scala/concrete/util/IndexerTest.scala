package concrete.util

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.Assertions
import scala.collection.SortedSet

class IndexerTest extends FlatSpec with Assertions {

  "Indexer.factory" should "use DirectIndices class" in {
    val i = Indexer.factory(SortedSet[Int]() ++ (0 to 10))

    assert(i.isInstanceOf[DirectIndices])
    assertResult(3)(i.index(3))
    assertResult(3)(i.value(3))
    assertResult(-1)(i.index(-1))
    assertResult(-1)(i.index(11))
  }

  "Indexer.factory" should "generate OffsetIndices class" in {
    val i = Indexer.factory(SortedSet[Int]() ++ (1 to 10))
    assert(i.isInstanceOf[OffsetIndices])
    assertResult(3)(i.index(4))
    assertResult(4)(i.value(3))
    assertResult(-1)(i.index(0))
    assertResult(-1)(i.index(11))
  }
}
