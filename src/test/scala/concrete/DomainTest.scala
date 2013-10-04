package concrete

import org.junit.Test
import org.junit.Assert._

class DomainTest {

  @Test
  def testRemove {
    val domain = IntDomain(10 to 20)

    domain.removeValInterval(13, 16)

    assertEquals(7, domain.size)

    assertTrue(domain.values.toSeq.sameElements((10 to 12) ++ (17 to 20)))

    val d2 = IntDomain(10 to 20)

    d2.removeValInterval(15, 25)

    assertTrue(d2.values.toSeq.sameElements(10 to 14))

    val d3 = IntDomain(10 to 20)
    d3.removeValInterval(5, 15)

    assertTrue(d3.values.toSeq.sameElements(16 to 20))

  }
}