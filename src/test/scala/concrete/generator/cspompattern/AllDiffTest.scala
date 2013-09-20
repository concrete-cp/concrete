package concrete.generator.cspompattern

import org.junit.Assert.assertEquals
import org.junit.Test
import cspom.CSPOM
import cspom.CSPOM._

class AllDiffTest {
  @Test
  def testExt() {
    val cspom = CSPOM {
      val v0 = varOf(1, 2, 3)
      val v1 = varOf(2, 3, 4)
      val v2 = varOf(1, 2, 3)
      val v3 = varOf(1, 2, 3)

      for (Seq(p1, p2) <- List(v0, v1, v2, v3).combinations(2)) {
        ctr(p1 ≠ p2)
      }
    }

    val allDiff = new AllDiff(cspom);
    val constraints = cspom.constraints.toSeq

    for (c <- constraints if (cspom.constraints.contains(c))) {
      allDiff.compile(c)
    }

    //println(cspom)
    assertEquals(1, cspom.constraints.size)
    assertEquals("allDifferent", cspom.constraints.head.description)
  }

}