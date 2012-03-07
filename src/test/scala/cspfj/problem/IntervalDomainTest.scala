package cspfj.problem
import org.junit.Test
import org.junit.Assert._

class IntervalDomainTest {

  @Test
  def test {
    val dom = new IntDomain((10 to 20).toArray)

    assertEquals(0, dom.first)
    assertEquals(10, dom.last)

    assertEquals((0 to 10).toSeq, dom.indices.toSeq)
    assertEquals((10 to 20).toSeq, dom.values.toSeq)

    dom.removeFrom(5)
    assertEquals(4, dom.last)

    dom.removeTo(3)
    assertEquals(4, dom.first)

    val bv = dom.getBitVector
    assertEquals(4, bv.nextSetBit(0))
    assertEquals(4, bv.prevSetBit(30))
  }
}